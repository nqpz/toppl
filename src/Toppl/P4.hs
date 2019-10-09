{-# LANGUAGE OverloadedStrings #-}
module Toppl.P4 where

import Data.Maybe (catMaybes)
import Data.Text.Prettyprint.Doc
import qualified Data.Set as S
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State

import Toppl.Base
import qualified Toppl.P5 as P5


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
                 , ruleContinue :: Maybe [Var]
                 }
  deriving (Show)

data Action = Unify Value Value
  deriving (Show)


instance VarsOf Action where
  varsOf (Unify a b) = varsOf a `S.union` varsOf b

instance VarsOf Rule where
  varsOf r = S.unions [ varsOf $ ruleParams r
                      , varsOf $ ruleBody r
                      , maybe S.empty varsOf $ ruleContinue r
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
  pretty (Unify a b) = prettyComposite "unify" [a, b]

type TransformerM = State Int

transform :: Prolog -> P5.Prolog
transform prolog = P5.Prolog (map transRG $ prologRuleGroups prolog)
  where transRG :: RuleGroup -> P5.RuleGroup
        transRG rg = evalState (P5.RuleGroup (rgPred rg)
                                <$> (catMaybes <$> mapM transRule (rgRules rg))
                                <*> pure (rgNParamsOrig rg) <*> pure (rgParamConvs rg)) 0

        transRule :: Rule -> TransformerM (Maybe P5.Rule)
        transRule r = do
          body <- transBody $ ruleBody r
          return (P5.Rule (ruleParams r) <$> body <*> pure (ruleContinue r))

        transBody :: [Action] -> TransformerM (Maybe [P5.Action])
        transBody as = fmap concat . lmToMl <$> mapM transAction as

        transAction :: Action -> TransformerM (Maybe [P5.Action])
        transAction (Unify a b) = case (a, b) of
          (Atom t, Atom u) -> toMaybeM (t == u) $ return $ Just []
          (Compound t ts, Compound u us) ->
            toMaybeM (t == u && length ts == length us)
            $ transBody $ zipWith Unify ts us
          (Variable tv@(Var tn ti), u) ->
            let dest = ActualVar tn ti
            in case u of
              Variable uv@(Var un ui) ->
                case tv == uv of
                  True -> return $ Just []
                  False ->
                    return $ Just [P5.Unify dest $ ActualVar un ui]
              Variable Wildcard{} -> return $ Just []
              Atom ua -> do
                dest0 <- uncurry ActualVar <$> newVar
                return $ Just [P5.Store dest0 ua [],
                               P5.Unify dest dest0]
              Compound ub us -> do
                dest0 <- uncurry ActualVar <$> newVar
                newVars <- replicateM (length us) newVar
                let body1 = [P5.Store dest0 ub (map (uncurry ActualVar) newVars),
                             P5.Unify dest dest0]
                body2 <- transBody $ zipWith Unify (map (Variable . uncurry Var) newVars) us
                return ((body1 ++) <$> body2)
          (Variable Wildcard{}, _) -> return $ Just []
          (_, Variable{}) -> transAction $ Unify b a
          _ -> return Nothing

        newVar :: TransformerM (Name, Int)
        newVar = do
          cur <- get
          put (cur + 1)
          return (Name Gen "Part", cur)

lmToMl :: [Maybe a] -> Maybe [a]
lmToMl = foldr (\e -> (>>= flip fmap e . flip (:))) (Just [])

toMaybeM :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
toMaybeM True m = m
toMaybeM False _ = return Nothing
