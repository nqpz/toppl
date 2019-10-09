{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Toppl.P5 where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import Control.Monad.Writer
import Control.Monad.State

import Toppl.Base
import qualified Toppl.P6 as P6


data Prolog = Prolog { prologRuleGroups :: [RuleGroup]
                     }
  deriving (Show)

data RuleGroup = RuleGroup { rgPred :: Predicate
                           , rgRules :: [Rule]
                           , rgNParamsOrig :: Int
                           , rgParamConvs :: [ParamConv]
                           }
  deriving (Show)

data Rule = Rule { ruleParams :: [Var]
                 , ruleBody :: [Action]
                 , ruleContinue :: Maybe [Var]
                 }
  deriving (Show)

data Action = Store ActualVar Text [ActualVar]
            | Unify ActualVar ActualVar
  deriving (Show)

instance VarsOf Action where
  varsOf (Store v _ ps) = varsOf v `S.union` varsOf ps
  varsOf (Unify a b) = varsOf a `S.union` varsOf b

instance VarsOf Rule where
  varsOf r = S.unions [ varsOf $ ruleParams r
                      , varsOf $ ruleBody r
                      , fromMaybe S.empty $ fmap varsOf $ ruleContinue r
                      ]

instance Pretty Prolog where
  pretty = vsep . map (vsep . (: [""]) . pretty) . prologRuleGroups

instance Pretty RuleGroup where
  pretty rg =
    vsep [ pretty (rgPred rg) <> slash <> pretty (rgNParamsOrig rg) <+>
           brackets (align $ fillSep $ punctuate comma $ map pretty $ rgParamConvs rg)
           <> colon
         , indent 2 $ vsep (map pretty (rgRules rg))
         ]

instance Pretty Rule where
  pretty r =
    let base = prettyComposite "" (ruleParams r)
    in case ruleBody r of
         [] -> base <> dot
         actions -> vsep ([ base <+> ":-"
                          , indent 2 (vsep (punctuate comma (map pretty actions)) <> dot)
                          ] ++ maybe [] ((: []) . indent 2 . ("->" <>)
                                         . prettyComposite "continue") (ruleContinue r))

instance Pretty Action where
  pretty (Store v t ps) =
    prettyComposite' "store" [pretty v, prettyComposite t ps]
  pretty (Unify a b) = prettyComposite "unify" [a, b]


transform :: Prolog -> P6.Prolog
transform prolog = P6.optimise $ useIntValuesAndVars (S.elems (findValues prolog)) prolog

-- Find atoms and names of compound values.  The Int is 0 for atoms and the
-- number of arguments for compound values.
findValues :: Prolog -> S.Set (Text, Int)
findValues = S.unions . map findRG . prologRuleGroups
  where findRG :: RuleGroup -> S.Set (Text, Int)
        findRG rg = S.unions (map findRule $ rgRules rg)
                    `S.union` S.unions (map findParamConv $ rgParamConvs rg)

        findRule :: Rule -> S.Set (Text, Int)
        findRule r = S.unions (map findAction $ ruleBody r)

        findAction :: Action -> S.Set (Text, Int)
        findAction (Store _ t ps) = S.singleton (t, length ps)
        findAction _ = S.empty

        findParamConv :: ParamConv -> S.Set (Text, Int)
        findParamConv (ValueParam v) = findValue v
        findParamConv IndexParam{} = S.empty

        findValue :: Value -> S.Set (Text, Int)
        findValue (Atom t) = S.singleton (t, 0)
        findValue Variable{} = S.empty
        findValue (Compound t vs) =
          S.unions (map findValue vs) `S.union` S.singleton (t, length vs)

useIntValuesAndVars :: [(Text, Int)] -> Prolog -> P6.Prolog
useIntValuesAndVars values prolog =
  P6.Prolog { P6.prologRuleGroups = map replaceRG $ prologRuleGroups prolog
            , P6.prologValueNames = A.array (1, length values) $ zip [1..] values
            }
  where valToInt = M.fromList $ zip values [1..]

        replaceRG :: RuleGroup -> P6.RuleGroup
        replaceRG rg = P6.RuleGroup { P6.rgPred = rgPred rg
                                    , P6.rgRules = map replaceRule $ rgRules rg
                                    , P6.rgNParamsOrig = rgNParamsOrig rg
                                    , P6.rgParamConvs = map replacePC $ rgParamConvs rg
                                    }

        replacePC :: ParamConv -> P6.ParamConv
        replacePC (ValueParam (Variable (Wildcard n))) = P6.Empty $ formatName n
        replacePC (ValueParam v) =
          P6.AtomParam $ replaceValue v
        replacePC (IndexParam i) = P6.IndexParam i

        replaceValue :: Value -> Int
        replaceValue (Atom t) = valToInt M.! (t, 0)
        replaceValue v@Compound{} = error ("no compound value should exist at this point: " ++ show v)
        replaceValue v@Variable{} = error ("no vars should be used in the paramconvs (transforming): " ++ show v)

        compactify :: (P6.Shape, [P6.ActualVar])
                   -> State (Int, M.Map P6.ActualVar P6.InitialVar) P6.InitialVar
        compactify (s, ps) = case ps of
          [] -> return (s, Nothing, Nothing)
          [p1] -> return (s, Just p1, Nothing)
          [p1, p2] -> return (s, Just p1, Just p2)
          (p1 : ps1) -> do
            i <- gets fst
            modify $ \(_, m) -> (i + 1, m)
            initVarSub <- compactify (0, ps1)
            modify $ \(i', m) -> (i', M.insert i initVarSub m)
            return (s, Just p1, Just i)

        replaceRule :: Rule -> P6.Rule
        replaceRule r =
          let (unifications, initialVars) = runWriter (concat <$> mapM replaceAction (ruleBody r))
              (initialVars1, (nVars, initialVars2)) =
                runState (mapM compactify (M.elems initialVars)) (length vars, M.empty)
          in P6.Rule { P6.ruleParams = map replaceVar $ ruleParams r
                     , P6.ruleInitialVars = M.fromList (zip (M.keys initialVars) initialVars1) `M.union` initialVars2
                     , P6.ruleNVars = nVars
                     , P6.ruleUnifications = unifications
                     , P6.ruleContinue = fmap (map replaceVar) $ ruleContinue r
                     , P6.ruleVarNames = A.array (0, length vars - 1) $ zip [0..]
                                         $ map (\(n, i) -> T.concat [ formatName n
                                                                    , "_"
                                                                    , T.pack (show i)
                                                                    ]) vars
                     }
          where vars = mapMaybe (\case
                                    Var n i -> Just (n, i)
                                    Wildcard{} -> Nothing)
                       $ S.elems $ varsOf r
                varToInt = M.fromList $ zip (map (\(n, i) -> Var n i) vars) [0..]

                replaceVar :: Var -> P6.Var
                replaceVar = \case
                  v@Var{} -> P6.Var (varToInt M.! v)
                  Wildcard name -> P6.Wildcard $ formatName name

                replaceVar' :: ActualVar -> P6.ActualVar
                replaceVar' (ActualVar n i) = varToInt M.! Var n i

                replaceAction :: Action -> Writer (M.Map P6.ActualVar (Int, [P6.ActualVar])) [(P6.ActualVar, P6.ActualVar)]
                replaceAction = \case
                  Store v t ps -> do
                    tell $ M.singleton (replaceVar' v) (valToInt M.! (t, length ps), map replaceVar' ps)
                    return []
                  Unify a b -> return [(replaceVar' a, replaceVar' b)]

        formatName :: Name -> Text
        formatName (Name ns t) = T.concat [ t
                                          , "_"
                                          , T.pack $ show ns
                                          ]
