{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Toppl.Base where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Set as S


type Error = String

data Namespace = Src -- ^ from the source
               | Gen -- ^ generated in a compiler pass
  deriving (Show, Eq, Ord)

data Name = Name Namespace Text
  deriving (Show, Eq, Ord)

-- | Used for most passes.
data Var = Var Name Int
         | Wildcard Name
  deriving (Show, Eq, Ord)

data ActualVar = ActualVar Name Int -- no wildcard support
  deriving (Show, Eq, Ord)

-- | Used for most passes.
data Value = Atom Text
           | Variable Var
           | Compound Text [Value]
  deriving (Show, Eq, Ord)

data Predicate = Predicate { predName :: Text
                           , predNParams :: Int
                           }
  deriving (Show, Eq, Ord)

-- | Used by some passes to map input to transformed predicates.
data ParamConv = ValueParam Value
               | IndexParam Int
  deriving (Show)

instance Pretty Namespace where
  pretty = pretty . show

instance Pretty Name where
  pretty (Name ns t) = pretty t <> colon <> pretty ns

instance Pretty Var where
  pretty = \case
    Var t i -> pretty t <> "#" <> pretty i
    Wildcard t -> "_" <> pretty t

instance Pretty ActualVar where
  pretty (ActualVar n i) = pretty (Var n i)

instance Pretty Value where
  pretty = \case
    Atom t -> pretty t
    Variable t -> pretty t
    Compound name parts -> prettyComposite name parts

instance Pretty Predicate where
  pretty (Predicate name nParams) = pretty name <> slash <> pretty nParams

instance Pretty ParamConv where
  pretty (ValueParam v) = pretty v
  pretty (IndexParam i) = "#" <> pretty i

prettyComposite :: Pretty val => Text -> [val] -> Doc ann
prettyComposite name values = prettyComposite' name (map pretty values)

prettyComposite' :: Text -> [Doc ann] -> Doc ann
prettyComposite' name [] = pretty name
prettyComposite' name values =
  pretty name <> parens (align (fillSep (punctuate comma values)))

findFix :: Eq a => (a -> a) -> a -> a
findFix step val | val' <- step val
                 , val' /= val = findFix step val'
                 | otherwise = val

render :: Pretty a => a -> Text
render = renderStrict .
         layoutSmart (LayoutOptions (AvailablePerLine 80 1)) .
         pretty

class VarsOf a where
  varsOf :: a -> S.Set Var

instance VarsOf Var where
  varsOf = S.singleton

instance VarsOf ActualVar where
  varsOf (ActualVar t i) = varsOf $ Var t i

instance VarsOf Value where
  varsOf = \case
    Atom{} -> S.empty
    Variable v -> varsOf v
    Compound _ vs -> varsOf vs

instance VarsOf ParamConv where
  varsOf (ValueParam v) = varsOf v
  varsOf _ = S.empty

instance VarsOf a => VarsOf [a] where
  varsOf = S.unions . map varsOf
