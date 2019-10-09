{-# LANGUAGE OverloadedStrings #-}
module Toppl.P3 where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import qualified Data.Set as S
import Control.Applicative ((<$>))
import Control.Monad.Writer
import Control.Monad.State

import Toppl.Base
import qualified Toppl.P4 as P4


newtype Prolog = Prolog { prologRuleGroups :: [RuleGroup]
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
                 }
  deriving (Show)

data Action = Unify Value Value
            | Recurse [Value]
  deriving (Show)


instance VarsOf Action where
  varsOf (Unify a b) = varsOf a `S.union` varsOf b
  varsOf (Recurse vs) = varsOf vs

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
         actions -> vsep [ base <+> ":-"
                         , indent 2 (vsep (punctuate comma (map pretty actions)) <> dot)
                         ]

instance Pretty Action where
  pretty (Unify a b) = prettyComposite "unify" [a, b]
  pretty (Recurse args) = prettyComposite "recurse" args

type TransformerM = State Int

stackifyRG :: RuleGroup -> (Bool, RuleGroup)
stackifyRG rg
  | hasRecurse (rgRules rg) =
    (True, rg { rgRules = map stackify $ rgRules rg
              , rgPred = (rgPred rg) { predNParams = predNParams (rgPred rg) + 2 }
              , rgParamConvs = ValueParam noStack : ValueParam listEmpty : rgParamConvs rg
              })
  | otherwise = (False, rg)

stackify :: Rule -> Rule
stackify r = r { ruleParams = noStackVar : stackVar : ruleParams r
               , ruleBody = Unify (Variable noStackVar) noStack
                            : ruleBody r
               }
-- hasRecurse = any (any isRecurseBase . ruleBody)

hasRecurse :: [Rule] -> Bool
hasRecurse = any hasRecurseRule

hasRecurseRule :: Rule -> Bool
hasRecurseRule r = case filter (isRecurseBase . fst)
                    $ zip (ruleBody r) (map (== length (ruleBody r)) [1..]) of
  [] -> False
  [(_, True)] -> False
  _ -> True

isRecurseBase :: Action -> Bool
isRecurseBase Recurse{} = True
isRecurseBase _ = False

transform :: Prolog -> P4.Prolog
transform prolog =
  let (forces, prolog') = unzip $ map stackifyRG (prologRuleGroups prolog)
      (prolog'', _) = until (hasLimitedRecurse forces . fst) (separate forces) (Prolog prolog', 0)
  in P4.Prolog $ evalState (zipWithM transformRG forces (prologRuleGroups prolog'')) 0

  where transformRG :: Bool -> RuleGroup -> TransformerM P4.RuleGroup
        transformRG force (RuleGroup p rs npo pcs) = do
          rs' <- mapM (transformRule p force) rs
          let rs'' = rs' ++ [baseCase p | force]
          return $ P4.RuleGroup p rs'' npo pcs

        baseCase p = P4.Rule { P4.ruleParams = [stackAtomVar, stackVar] ++ replicate (predNParams p - 2) (Wildcard (Name Gen "ListEmptyCase"))
                             , P4.ruleBody = [ P4.Unify (Variable stackAtomVar) stackAtom
                                             , P4.Unify (Variable stackVar) listEmpty
                                             ]
                             , P4.ruleContinue = Nothing
                             }

        transformRule :: Predicate -> Bool -> Rule -> TransformerM P4.Rule
        transformRule _ _ (Rule params []) = return $ P4.Rule params [] Nothing
        transformRule p forceContinue (Rule params body) = do
          let (body', continue) = case (forceContinue, last body) of
                (True, Recurse args) -> (init body, Just args)
                (False, Recurse args) -> (init body, Just args)
                _ -> (body, Nothing)
          (continue', continueUnifications) <- case continue of
            Nothing -> return $ case forceContinue of
              True -> (Just ([stackAtomVar, stackVar] ++ replicate (predNParams p - 2) (Wildcard $ Name Gen "ContinueStack")), [P4.Unify (Variable stackAtomVar) stackAtom])
              False -> (Nothing, [])
            Just vals -> do
              vars <- mapM newVar vals
              return (Just vars, zipWith P4.Unify (map Variable vars) vals)
          return $ P4.Rule params (map transformAction body' ++ continueUnifications) continue'

        newVar :: Value -> TransformerM Var
        newVar (Variable v) = return v
        newVar _ = do
          cur <- get
          put (cur + 1)
          return $ Var (Name Gen "ContinueVar") cur

        transformAction :: Action -> P4.Action
        transformAction (Unify a b) = P4.Unify a b
        transformAction Recurse{} = error "impossible"

type SepInnerM = State Int
type SepM = WriterT [Rule] SepInnerM

listEmpty :: Value
listEmpty = Atom "#list_empty"

stackVar :: Var
stackVar = Var (Name Gen "Stack") 0

noStackVar :: Var
noStackVar = Var (Name Gen "NoStack") 0

noStack :: Value
noStack = Atom "#no_stack"

stackAtom :: Value
stackAtom = Atom "#stack"

stackAtomVar :: Var
stackAtomVar = Var (Name Gen "StackAtom") 0

separate :: [Bool] -> (Prolog, Int) -> (Prolog, Int)
separate forces (prolog, maxId) =
  runState (Prolog <$> zipWithM sepRG forces (prologRuleGroups prolog)) maxId
  where sepRG :: Bool -> RuleGroup -> SepInnerM RuleGroup
        sepRG force rg | force = do
                           (rules', newRules) <-
                             runWriterT $ mapM (sepRule force) $ rgRules rg
                           return $ extendWithWildcards (predNParams (rgPred rg))
                             rg { rgRules = rules' ++ newRules }
                       | otherwise = return rg

        sepRule :: Bool -> Rule -> SepM Rule
        sepRule force r = case break isRecurse (ruleBody r) of
          (_as0, []) -> return r
          (as0, [Recurse args]) -> if force
                                   then return r { ruleBody = as0 ++ [Recurse (noStack : Variable stackVar : args)] }
                                   else return r { ruleBody = as0 ++ [Recurse args] }
          (as0, Recurse args : as1) -> do
            as0' <- sepRule' as0 args as1
            return r { ruleBody = as0' }
          _ -> error "impossible"

        sepRule' :: [Action] -> [Value] -> [Action] -> SepM [Action]
        sepRule' as0 args as1 = do
          curId <- get
          let recurseVars = S.elems (varsOf as1)
              identCompound = Compound (T.append "#continue_" (T.pack $ show curId)) (map Variable recurseVars)
              identVar = Var (Name Gen "Continue") curId
              stack' = Compound "#list" [identCompound, Variable stackVar]
          put (curId + 1)
          newRule $ Rule { ruleParams = [stackAtomVar, Var (Name Gen "SuperStack") 0]
                         , ruleBody = Unify (Variable stackAtomVar) stackAtom
                                      : Unify (Variable (Var (Name Gen "SuperStack") 0)) (Compound "#list" [identCompound, Variable stackVar])
                                      : Unify (Variable identVar) identCompound
                                      : as1
                         }
          return (as0 ++ [Recurse (noStack : stack' : args)])

        newRule = tell . (: [])

extendWithWildcards :: Int -> RuleGroup -> RuleGroup
extendWithWildcards nArgs rg = rg { rgPred = extendPred $ rgPred rg
                                  , rgRules = map extendRule $ rgRules rg
                                  , rgParamConvs = extendParamConvs $ rgParamConvs rg
                                  }
  where extendPred :: Predicate -> Predicate
        extendPred p = p { predNParams = nArgs }

        extendRule :: Rule -> Rule
        extendRule r = r { ruleParams = ruleParams r ++
                                        missingWildcards (length (ruleParams r))
                         , ruleBody = map extendAction $ ruleBody r
                         }

        extendAction :: Action -> Action
        extendAction u@Unify{} = u
        extendAction (Recurse args) = Recurse (args ++ map Variable (missingWildcards (length args)))

        extendParamConvs :: [ParamConv] -> [ParamConv]
        extendParamConvs pcs = pcs ++ map (ValueParam . Variable) (missingWildcards (length pcs))

        missingWildcards :: Int -> [Var]
        missingWildcards len =
          replicate (nArgs - len) $ Wildcard $ Name Gen "Extended"


-- Is there at most one Recurse action at the end of all predicate rule bodies?
hasLimitedRecurse :: [Bool] -> Prolog -> Bool
hasLimitedRecurse forces prolog = all (all (okActions . ruleBody) . rgRules)
                                  $ map snd $ filter fst $ zip forces $ prologRuleGroups prolog
  where okActions :: [Action] -> Bool
        okActions = not . any isRecurse

isRecurse :: Action -> Bool
isRecurse (Recurse (arg : _)) = arg /= noStack
isRecurse (Recurse []) = False
isRecurse _ = False
