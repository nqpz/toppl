{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Toppl.P2 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Writer

import Toppl.Base
import qualified Toppl.P3 as P3


newtype Prolog = Prolog { prologRuleGroups :: [RuleGroup] }
  deriving (Show)

data RuleGroup = RuleGroup { rgPred :: Predicate
                           , rgRules :: [Rule]
                           }
  deriving (Show)

data Rule = Rule { ruleParams :: [Value]
                 , ruleBody :: [Call]
                 }
  deriving (Show)

data Call = Call { callName :: Text
                 , callArgs :: [Value]
                 }
  deriving (Show)


instance Pretty Prolog where
  pretty = vsep . map (vsep . (: [""]) . pretty) . prologRuleGroups

instance Pretty RuleGroup where
  pretty rg =
    vsep [ pretty (rgPred rg) <> colon
         , indent 2 $ vsep (map pretty (rgRules rg))
         ]

instance Pretty Rule where
  pretty r =
    let base = prettyComposite "" (ruleParams r)
    in case ruleBody r of
         [] -> base <> dot
         calls -> vsep [ base <+> ":-"
                       , indent 2 (vsep (punctuate comma (map pretty calls)) <> dot)
                       ]

instance Pretty Call where
  pretty c = prettyComposite (callName c) (callArgs c)

type CFG = M.Map Predicate (S.Set Predicate)

newtype CFG' = CFG' { unCFG' :: CFG }

instance Semigroup CFG' where
  CFG' m <> CFG' n = CFG' $ M.unionWith S.union m n

instance Monoid CFG' where
  mempty = CFG' M.empty
  mappend = (<>)

type CFGM = Writer CFG'

buildCFG :: Prolog -> CFG
buildCFG (Prolog ruleGroups) =
  findFix extend $ unCFG'
  $ execWriter $ mapM_ checkRuleGroup ruleGroups
  where checkRuleGroup :: RuleGroup -> CFGM ()
        checkRuleGroup rg = mapM_ (mapM_ (checkCall (rgPred rg)) . ruleBody)
                            $ rgRules rg

        checkCall :: Predicate -> Call -> CFGM ()
        checkCall predCur c = tell $ CFG' $ M.singleton predCur $ S.singleton
                              $ Predicate (callName c) (length $ callArgs c)

        extend :: CFG -> CFG
        extend cfg = M.map (\preds -> S.union preds
                                      (S.unions (S.map predPreds preds))) cfg
          where predPreds p = fromMaybe S.empty (M.lookup p cfg)

isRecursive :: CFG -> Predicate -> Bool
isRecursive cfg p = p `S.member` fromMaybe S.empty (M.lookup p cfg)


transform :: Prolog -> P3.Prolog
transform prolog = P3.Prolog $ map transRuleGroup $ prologRuleGroups prolog
  where cfg :: CFG
        cfg = buildCFG prolog

        transRuleGroup :: RuleGroup -> P3.RuleGroup
        transRuleGroup rg =
          let predsUsedAll = fromMaybe S.empty (M.lookup (rgPred rg) cfg)
              recPredsUsed = S.filter (isRecursive cfg) predsUsedAll
              predsUsed = S.insert (rgPred rg) recPredsUsed
              maxNParams = S.findMax $ S.map predNParams predsUsed
              simple = null recPredsUsed || recPredsUsed == S.singleton (rgPred rg)
              predRepls = M.fromSet (predRepl simple maxNParams) predsUsed
              rg' = rg { rgPred = Predicate (predName $ rgPred rg) (maxNParams + (if simple then 0 else 1))
                       , rgRules = map (replaceParams (applyRepl (predRepls M.! rgPred rg))) (rgRules rg) ++
                                   concatMap (addRule predRepls)
                                   (S.elems (S.delete (rgPred rg) recPredsUsed))
                       }
          in replaceNonrecursiveCalls (rgPred rg) predRepls rg'

        predRepl :: Bool -> Int -> Predicate -> ([Value], [Value])
        predRepl True _ _ = ([], [])
        predRepl False maxNParams callee =
          let identAtom = Atom $ T.concat [ "#ident_"
                                          , predName callee
                                          , "_"
                                          , T.pack $ show $ predNParams callee
                                          ]
              fillerWildcards = replicate (maxNParams - predNParams callee)
                                (Variable $ Wildcard $ Name Gen "Filler")
          in ([identAtom], fillerWildcards)

        applyRepl :: ([Value], [Value]) -> ([Value] -> [Value])
        applyRepl (start, end) vs = start ++ vs ++ end

        addRule :: M.Map Predicate ([Value], [Value]) -> Predicate -> [Rule]
        addRule repls p =
          let rulesOrig = fromMaybe (error "impossible")
                          $ L.lookup p $ map (\rg -> (rgPred rg, rgRules rg))
                          $ prologRuleGroups prolog
          in map fixRule rulesOrig
          where fixRule :: Rule -> Rule
                fixRule r = r { ruleParams = applyRepl (repls M.! p) $ ruleParams r }

        replaceParams :: ([Value] -> [Value]) -> Rule -> Rule
        replaceParams callerRepl r = r { ruleParams = callerRepl $ ruleParams r }

        replaceNonrecursiveCalls :: Predicate -> M.Map Predicate ([Value], [Value])
                                 -> RuleGroup -> P3.RuleGroup
        replaceNonrecursiveCalls basePred repls (RuleGroup p rules) =
          P3.RuleGroup p (concat $ zipWith replRule rules [0..]) (predNParams basePred) paramConvs
          where replRule :: Rule -> Int -> [P3.Rule]
                replRule r nthRule =
                  let (params, initialUnifications) =
                        unzip $ zipWith (\i -> \case
                                            Variable v -> (v, [])
                                            val -> let var = Var (Name Gen
                                                                  (T.concat [ "Param_"
                                                                            , predName basePred
                                                                            , "_"
                                                                            , T.pack (show (predNParams basePred))
                                                                            , "_"
                                                                            , T.pack (show i)
                                                                            ])) nthRule
                                                   in (var, [P3.Unify (Variable var) val])
                                    ) [(0::Int)..] (ruleParams r)
                  in map (P3.Rule params . (concat initialUnifications ++)) $ replBody $ ruleBody r

                replBody :: [Call] -> [[P3.Action]]
                replBody [] = [[]]
                replBody (call : calls) =
                  let callPred = Predicate (callName call) (length $ callArgs call)
                  in case M.lookup callPred repls of
                    Just repl -> map (P3.Recurse (applyRepl repl (callArgs call)) :) (replBody calls)
                    Nothing -> concatMap (\rest -> map (++ rest)
                                                   $ unify callPred (callArgs call)) (replBody calls)

                unify :: Predicate -> [Value] -> [[P3.Action]]
                unify call args = concatMap unifyRule $ ruleGroupMap M.! call
                  where unifyRule :: Rule -> [[P3.Action]]
                        unifyRule rule = map (zipWith P3.Unify (ruleParams rule) args ++)
                                         $ replBody (ruleBody rule)

                paramConvs :: [ParamConv]
                paramConvs =
                  let base = map IndexParam [0..predNParams basePred - 1]
                      (start, ends) = repls M.! basePred
                  in map ValueParam start ++ base ++ map ValueParam ends

        ruleGroupMap :: M.Map Predicate [Rule]
        ruleGroupMap = M.fromList $ map (\rg -> (rgPred rg, rgRules rg)) $ prologRuleGroups prolog
