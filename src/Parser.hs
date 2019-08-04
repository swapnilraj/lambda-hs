{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed sc integer

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "+", "-", "/", "*" ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else pure x

variable :: Parser Expr
variable = do
  x <- identifier
  pure (Var x)

number :: Parser Expr
number = do
  n <- signedInteger
  pure $ Lit n

lambda :: Parser Expr
lambda = do
  symbol "\\"
  parameter <- variable
  symbol "."
  e <- term
  pure $ Abs parameter e

expr :: Parser Expr
expr = do
  es <- many term
  return (foldl1 App es)

term :: Parser Expr
term = parens expr
  <|> variable
  <|> lambda
  <|> number
