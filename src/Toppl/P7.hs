{-# LANGUAGE OverloadedStrings #-}
module Toppl.P7 where

import Data.Text.Prettyprint.Doc

import Toppl.P6 hiding (Prolog)
import qualified Toppl.P6 as P6


newtype Prolog = Prolog { prologProlog :: P6.Prolog }

instance Pretty Prolog where
  pretty = vsep . map (vsep . (: [""]) . pretty) . prologRuleGroups . prologProlog
