{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Toppl.Parser where

import Data.Char (isAlphaNum)
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Toppl.Base hiding (Var(..), Value(..))
import Toppl.P0
import qualified Toppl.Interpreter as Interpreter


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "%")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

pValue :: Parser Value
pValue = pVariable <|> pCompoundOrAtom
  where pVariable = Variable <$> pUpperStart
        pCompoundOrAtom = do
          start <- pLowerStart
          let pCompound = Compound start <$> parens (commaSep pValue)
          try pCompound <|> pure (Atom start)

isValidChar :: Char -> Bool
isValidChar c = isAlphaNum c || c == '_'

pUpperStart :: Parser Text
pUpperStart = lexeme $ T.cons <$> (upperChar <|> char '_') <*> takeWhileP Nothing isValidChar

pLowerStart :: Parser Text
pLowerStart = lexeme $ T.cons <$> lowerChar <*> takeWhileP Nothing isValidChar

pComposite :: Parser (Text, [Value])
pComposite = (,) <$> pLowerStart <*> (parens (commaSep pValue) <|> return [])

pRule :: Parser Rule
pRule = do
  (name, params) <- pComposite
  body <- pFact <|> pRule'
  return $ Rule name params body
  where pFact = symbol "." *> return []
        pRule' = symbol ":-" *> commaSep pCall <* symbol "."

pCall :: Parser Call
pCall = uncurry Call <$> pComposite

pProlog :: Parser Prolog
pProlog = Prolog <$> (sc *> many pRule <* eof)

transform :: (Text, FilePath) -> Either Error Prolog
transform (input, fname) =
  case runParser pProlog fname input of
    Left bundle -> Left $ errorBundlePretty bundle
    Right p -> Right p

pQuery :: Parser Interpreter.Query
pQuery = do
  (p, args) <- pComposite <* symbol "."
  let args' = map transValue args
  return $ Interpreter.Query p args'

transValue :: Value -> Interpreter.Value
transValue = \case
  Atom t -> Interpreter.Atom t
  Compound t ps -> Interpreter.Compound t $ map transValue ps
  Variable t -> case T.head t of
    '_' -> Interpreter.Wildcard $ T.tail t
    _ -> Interpreter.Var t

pQueries :: Parser [Interpreter.Query]
pQueries = space *> many pQuery <* eof

pPredicate :: Parser Predicate
pPredicate = do
  name <- pLowerStart
  void $ symbol "/"
  nParams <- L.decimal
  return $ Predicate name nParams

pPredicates :: Parser [Predicate]
pPredicates = space *> commaSep pPredicate <* eof

parseQuery :: Text -> Either Error Interpreter.Query
parseQuery input = case runParser pQuery "<input>" input of
  Left bundle -> Left $ errorBundlePretty bundle
  Right p -> Right p

parseQueries :: Text -> Either Error [Interpreter.Query]
parseQueries input = case runParser pQueries "<input>" input of
  Left bundle -> Left $ errorBundlePretty bundle
  Right p -> Right p

parsePredicates :: Text -> Either Error [Predicate]
parsePredicates input = case runParser pPredicates "<input>" input of
  Left bundle -> Left $ errorBundlePretty bundle
  Right p -> Right p

pResultMapping :: Parser (Text, Interpreter.Value)
pResultMapping = do
  var <- pUpperStart
  void $ symbol "="
  val <- pValue
  return (var, transValue val)

braces :: Parser a -> Parser a
braces = between (symbol "{") (string "}")

pResult :: Parser Interpreter.Result
pResult = do
  void $ string "yes"
  Interpreter.Result <$> (braces (M.fromList <$> pResultMapping `sepBy` symbol ",") <|> return M.empty)

pResults :: Parser [Interpreter.Result]
pResults = do
  try (string "no\n" >> return []) <|> pResult `sepEndBy` string "\n"

pQA :: Parser (Interpreter.Query, [Interpreter.Result])
pQA = do
  q <- pQuery
  rs <- pResults
  return (q, rs)

pQAs :: Parser [(Interpreter.Query, [Interpreter.Result])]
pQAs = space *> pQA `sepBy` string "\n" <* eof

parseQAs :: (Text, FilePath) -> Either Error [(Interpreter.Query, [Interpreter.Result])]
parseQAs (input, fname) = case runParser pQAs fname input of
  Left bundle -> Left $ errorBundlePretty bundle
  Right p -> Right p

parseResults :: Text -> Either Error [Interpreter.Result]
parseResults input = case runParser pResults "<input>" input of
  Left bundle -> Left $ errorBundlePretty bundle
  Right p -> Right p


