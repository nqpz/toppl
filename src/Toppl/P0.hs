{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Toppl.P0 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State

import Toppl.Base hiding (Var(..), Value(..))
import qualified Toppl.Base as Base
import qualified Toppl.P1 as P1


data Prolog = Prolog { prologRules :: [Rule] }
  deriving (Show)

data Rule = Rule { ruleName :: Text
                 , ruleParams :: [Value]
                 , ruleBody :: [Call]
                 }
  deriving (Show)

data Value = Atom Text
           | Variable Text
           | Compound Text [Value]
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

instance Pretty Value where
  pretty = \case
    Atom t -> pretty t
    Variable t -> pretty t
    Compound name parts -> prettyComposite name parts

instance Pretty Call where
  pretty c = prettyComposite (callName c) (callArgs c)


type NamerM = State (M.Map Text Int)

transform :: Prolog -> P1.Prolog
transform (Prolog rules) =
  P1.Prolog $ evalState (mapM transRule rules) M.empty
  where transRule :: Rule -> NamerM P1.Rule
        transRule (Rule name params body) = do
          rule <- P1.Rule name <$> mapM transValue params <*> mapM transCall body
          incIds
          return rule

        transValue :: Value -> NamerM Base.Value
        transValue = \case
          Atom t -> return $ Base.Atom t
          Variable name -> Base.Variable <$> case T.head name of
            '_' -> return $ Base.Wildcard $ Base.Name Base.Src (T.tail name)
            _ -> do
              i <- curId name
              return $ Base.Var (Base.Name Base.Src name) i
          Compound t vs -> Base.Compound t <$> mapM transValue vs

        transCall :: Call -> NamerM P1.Call
        transCall (Call name args) = P1.Call name <$> mapM transValue args

        curId :: Text -> NamerM Int
        curId name = do
          cur <- gets (M.lookup name)
          case cur of
            Nothing -> do
              modify $ M.insert name 0
              return 0
            Just i -> return i

        incIds :: NamerM ()
        incIds = modify $ M.map (+ 1)
