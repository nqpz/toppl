{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Toppl.P6 where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Array (Array)
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

import Toppl.Base hiding (Var(..), ActualVar(..), Value(..), ParamConv(..))


data Var = Var Int
         | Wildcard Text
  deriving (Show)

type ActualVar = Int -- no wildcard support

type Shape = Int -- XXX: better name

data ParamConv = AtomParam Int
               | IndexParam Int
               | Empty Text -- wildcard
  deriving (Show)

data Prolog = Prolog { prologRuleGroups :: [RuleGroup]
                     , prologValueNames :: Array Int (Text, Int)
                     }
  deriving (Show)

data RuleGroup = RuleGroup { rgPred :: Predicate
                           , rgRules :: [Rule]
                           , rgNParamsOrig :: Int
                           , rgParamConvs :: [ParamConv]
                           }
  deriving (Show)

type InitialVar = (Shape, Maybe ActualVar, Maybe ActualVar)

data Rule = Rule { ruleParams :: [Var]
                 , ruleInitialVars :: M.Map ActualVar InitialVar
                 , ruleNVars :: Int
                 , ruleUnifications :: [(ActualVar, ActualVar)]
                 , ruleContinue :: Maybe [Var]
                 , ruleVarNames :: Array Int Text -- ^ for debugging
                 }
  deriving (Show)

instance Pretty Prolog where
  pretty prolog = vsep $ map (vsep . (: [""]) . prettyRuleGroup) $ prologRuleGroups prolog
    where prettyRuleGroup rg =
            vsep [ pretty (rgPred rg) <> slash <> pretty (rgNParamsOrig rg) <+>
                   brackets (align $ fillSep $ punctuate comma
                             $ map prettyParamConv $ rgParamConvs rg)
                   <> colon
                 , indent 2 $ vsep (map prettyRule (rgRules rg))
                 ]

          intName 0 = "0"
          intName i = brackets (pretty i) <> pretty (fst (prologValueNames prolog A.! i))

          prettyParamConv :: ParamConv -> Doc ann
          prettyParamConv (AtomParam i) = intName i
          prettyParamConv (IndexParam i) = "#" <> pretty i
          prettyParamConv (Empty t) = "_" <> pretty t

          prettyRule r =
            let base = vsep [ braces (pretty (ruleNVars r) <+> "vars")
                            , "initial vars: " <> braces (align (fillSep (punctuate comma (map prettyInitialVar (M.assocs (ruleInitialVars r))))))
                            , prettyComposite' "" (map prettyVar $ ruleParams r)
                            ]
            in case ruleUnifications r of
              [] -> base <> dot
              unis -> vsep ([ base <+> ":-"
                            , indent 2 (vsep (punctuate comma
                                              (map prettyUni unis)) <> dot)
                            ] ++ maybe [] ((: []) . indent 2 . ("->" <>)
                                           . prettyComposite' "continue")
                            (map prettyVar <$> ruleContinue r))

            where prettyVar = \case
                    Wildcard t -> "_" <> pretty t
                    Var i -> prettyVar' i

                  prettyVar' i
                    | i <= snd (A.bounds (ruleVarNames r)) = brackets (pretty i) <> pretty (ruleVarNames r A.! i)
                    | otherwise = pretty i

                  prettyUni (a, b) = prettyComposite' "unify" [prettyVar' a, prettyVar' b]

                  prettyInitialVar (addr, (s, p1, p2)) =
                    pretty addr <> equals <> intName s <> prettyComposite' ""
                    (map prettyVar' $ catMaybes [p1, p2])

-- Compact pretty printings:

instance Pretty RuleGroup where
  pretty rg = vsep [ pretty (rgPred rg) <> slash <> pretty (rgNParamsOrig rg) <+>
                     brackets (align $ fillSep $ punctuate comma
                               $ map pretty $ rgParamConvs rg)
                     <> colon
                   , indent 2 $ vsep (map pretty (rgRules rg))
                   ]

instance Pretty ParamConv where
  pretty = \case
    AtomParam i -> pretty i <> parens ""
    IndexParam i -> "#" <> pretty i
    Empty _ -> "_"

instance Pretty Rule where
  pretty r =
    let base = vsep [ braces (pretty (ruleNVars r) <+> "vars")
                    , "initial vars: " <> braces (align (fillSep (punctuate comma (map prettyInitialVar (M.assocs (ruleInitialVars r))))))
                    , prettyComposite' "" (map pretty $ ruleParams r)
                    ]
    in case ruleUnifications r of
         [] -> base <> dot
         unis -> vsep ([ base <+> ":-"
                       , indent 2 (fillSep (punctuate comma
                                            (map (\(a, b) -> pretty a <> equals <> pretty b) unis)) <> dot)
                            ] ++ maybe [] ((: []) . indent 2 . ("->" <>)
                                           . prettyComposite' "continue")
                            (map pretty <$> ruleContinue r))
    where prettyInitialVar (addr, (s, p1, p2)) =
            pretty addr <> equals <> pretty s <> prettyComposite' "" (map pretty $ catMaybes [p1, p2])

instance Pretty Var where
  pretty = \case
    Var i -> pretty i
    Wildcard _ -> "_"


-- Fuse intermediate unifications.
optimise :: Prolog -> Prolog
optimise p = p { prologRuleGroups = map optimiseRG $ prologRuleGroups p }

optimiseRG :: RuleGroup -> RuleGroup
optimiseRG rg = rg { rgRules = map optimiseRule $ rgRules rg }

optimiseRule :: Rule -> Rule
optimiseRule r =
  let coreAddrs = S.fromList (concatMap (\case
                                            Var i -> [i]
                                            Wildcard{} -> []) (ruleParams r)
                              ++ M.keys (ruleInitialVars r)
                              ++ concatMap (\(_, a, b) -> catMaybes [a, b])
                              (M.elems (ruleInitialVars r)))
      m = catMaybes <$> mapM (simplifyUnification coreAddrs) (ruleUnifications r)
      (unifications', (replacements, _undecided)) = runState m (M.empty, [])
      r' = r { ruleUnifications = unifications'
             , ruleContinue = map (replaceContinueVar replacements) <$> ruleContinue r }
      r'' = wildcardContinueVars r'
  in compactNVars r''

lookup' :: ActualVar -> [(ActualVar, ActualVar)]
        -> Maybe (ActualVar, [(ActualVar, ActualVar)])
lookup' _ [] = Nothing
lookup' t ((a, b) : xs)
  | t == a = Just (b, xs)
  | t == b = Just (a, xs)
  | otherwise = do
      (res, xs') <- lookup' t xs
      return (res, (a, b) : xs')

simplifyUnification :: S.Set ActualVar -> (ActualVar, ActualVar)
                    -> State (M.Map ActualVar ActualVar, [(ActualVar, ActualVar)])
                       (Maybe (ActualVar, ActualVar))
simplifyUnification coreAddrs (a, b) = case (a `S.member` coreAddrs, b `S.member` coreAddrs) of
  (True, True) -> return $ Just (a, b)
  (True, False) -> tryExtend a b
  (False, True) -> tryExtend b a
  (False, False) -> do
    (m, undecided) <- get
    case (M.lookup a m, M.lookup b m) of
      (Just a', Just b') -> return $ Just (a', b')
      (Just a', Nothing) -> do
        case lookup' b undecided of
          Just (b', undecided') -> put (M.insert b a' $ M.insert b' a' m, undecided')
          Nothing -> put (M.insert b a' m, undecided)
        return Nothing
      (Nothing, Just b') -> do
        case lookup' a undecided of
          Just (a', undecided') -> put (M.insert a b' $ M.insert a' b' m, undecided')
          Nothing -> put (M.insert a b' m, undecided)
        return Nothing
      (Nothing, Nothing) -> do
        put (m, (a, b) : undecided)
        return Nothing

tryExtend :: ActualVar -> ActualVar
          -> State (M.Map ActualVar ActualVar, [(ActualVar, ActualVar)]) (Maybe (ActualVar, ActualVar))
tryExtend a b = do
  (m, undecided) <- get
  case M.lookup b m of
    Just b' -> return $ Just (a, b')
    Nothing -> do
      case lookup' b undecided of
        Just (b', undecided') -> put (M.insert b a $ M.insert b' a m, undecided')
        Nothing -> put (M.insert b a m, undecided)
      return Nothing

replaceContinueVar :: M.Map ActualVar ActualVar -> Var -> Var
replaceContinueVar repl (Var i) = Var $ fromMaybe i $ M.lookup i repl
replaceContinueVar _ w@Wildcard{} = w

-- Ensure there are no "holes".
compactNVars :: Rule -> Rule
compactNVars r =
  let addrs = L.sort (L.nub (concatMap (\case
                                           Var i -> [i]
                                           Wildcard{} -> []) (ruleParams r)
                             ++ M.keys (ruleInitialVars r)
                             ++ concatMap (\(_, a, b) -> catMaybes [a, b])
                             (M.elems (ruleInitialVars r))
                             ++ map fst (ruleUnifications r)
                             ++ map snd (ruleUnifications r)))
      compact = (M.fromList (zip addrs [0..]) M.!)
      addrsWithNames = map (ruleVarNames r A.!) (takeWhile (<= snd (A.bounds (ruleVarNames r))) addrs)
  in r { ruleParams = map (compactVar compact) $ ruleParams r
       , ruleInitialVars = M.fromList (zip
                                       (map compact (M.keys (ruleInitialVars r)))
                                       (map (\(s, a, b) -> (s, compact <$> a, compact <$> b))
                                        (M.elems (ruleInitialVars r))))
       , ruleNVars = length addrs
       , ruleUnifications = map (\(a, b) -> (compact a, compact b)) $ ruleUnifications r
       , ruleContinue = map (compactVar compact) <$> ruleContinue r
       , ruleVarNames = A.array (0, length addrsWithNames - 1) $ zip [0..] addrsWithNames
       }

compactVar :: (ActualVar -> ActualVar) -> Var -> Var
compactVar compact = \case
  Var i -> Var $ compact i
  w@Wildcard{} -> w

wildcardContinueVars :: Rule -> Rule
wildcardContinueVars r = r { ruleContinue = map check <$> ruleContinue r}
  where check :: Var -> Var
        check (Var i) | i `S.member` addrs = Var i
                      | otherwise = Wildcard (T.append "NotUsed_" (T.pack (show i)))
        check w@Wildcard{} = w

        addrs = S.fromList (concatMap (\case
                                          Var i -> [i]
                                          Wildcard{} -> []) (ruleParams r)
                            ++ M.keys (ruleInitialVars r)
                            ++ concatMap (\(_, a, b) -> catMaybes [a, b])
                            (M.elems (ruleInitialVars r))
                            ++ map fst (ruleUnifications r)
                            ++ map snd (ruleUnifications r))
