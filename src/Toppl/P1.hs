{-# LANGUAGE OverloadedStrings #-}
module Toppl.P1 where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad.State

import Toppl.Base
import qualified Toppl.P2 as P2


data Prolog = Prolog { prologRules :: [Rule] }
  deriving (Show)

data Rule = Rule { ruleName :: Text
                 , ruleParams :: [Value]
                 , ruleBody :: [Call]
                 }
  deriving (Show)

data Call = Call { callName :: Text
                 , callArgs :: [Value]
                 }
  deriving (Show)


instance Pretty Prolog where
  pretty = vsep . map (vsep . (: [""]) . pretty) . prologRules

instance Pretty Rule where
  pretty r =
    let base = prettyComposite (ruleName r) (ruleParams r)
    in case ruleBody r of
         [] -> base <> dot
         calls -> vsep [ base <+> ":-"
                       , indent 2 (vsep (punctuate comma (map pretty calls)) <> dot)
                       ]

instance Pretty Call where
  pretty c = prettyComposite (callName c) (callArgs c)


type GroupM = State (Int, M.Map Predicate (Int, [P2.Rule]))

transform :: Prolog -> P2.Prolog
transform (Prolog rules) =
  let groups = sortBy (comparing (fst . snd)) $ M.assocs $ snd
               $ execState (mapM_ transRule rules) (0, M.empty)
  in P2.Prolog $ map (\(p, (_, locRules)) -> P2.RuleGroup p locRules) groups
  where transRule :: Rule -> GroupM ()
        transRule (Rule name params body) = do
          (nGroups, gm) <- get
          let query = Predicate name (length params)
              rule = P2.Rule params (map transCall body)
              locRules = M.lookup query gm
          case locRules of
            Nothing ->
              put (nGroups + 1, M.insert query (nGroups, [rule]) gm)
            Just (i, rules') ->
              put (nGroups, M.insert query (i, rules' ++ [rule]) gm)

        transCall :: Call -> P2.Call
        transCall (Call name args) = P2.Call name args
