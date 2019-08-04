module Parser where

import AST

import Control.Monad
import Control.Monad.Combinators.Expr
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

rword :: String -> Parser String
rword w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "lambda", "\\", ".", "true", "false"
      , "&&", "||", "!", "+", "-", "/", "*" ]

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

true, false, boolean :: Parser Expr
true = do rword "true"; pure (Lit $ LBool True)
false = do rword "false"; pure (Lit $ LBool False)
boolean = true <|> false

number :: Parser Expr
number = do
  n <- integer
  pure $ Lit (LInt n)

lambda :: Parser Expr
lambda = do
  symbol "\\" <|> rword "lambda"
  parameter <- variable
  symbol "."
  e <- expr
  pure $ Abs parameter e

term :: Parser Expr
term = try arithExpr
  <|> try booleanExpr
  <|> number
  <|> boolean
  <|> lambda
  <|> parens expr
  <|> variable

expr :: Parser Expr
expr = do
  es <- many term
  pure (foldl1 App es)

arithTerm :: Parser Expr
arithTerm = choice
  [ parens arithExpr
  , number
  , variable
  ]

arithOperations :: [[ Operator Parser Expr ]]
arithOperations =
  [
    [ Prefix (Negation <$ symbol "-")
    , Prefix (id <$ symbol "+")
    ]
  ,
    [ InfixL (Product <$ symbol "*")
    , InfixL (Division <$ symbol "/")
    ]
  ,
    [ InfixL (Sum <$ symbol "+")
    , InfixL (Substract <$ symbol "-")
    ]
  ]

arithExpr :: Parser Expr
arithExpr = makeExprParser arithTerm arithOperations

booleanTerm :: Parser Expr
booleanTerm = choice
  [ parens booleanExpr
  , boolean
  , variable
  ]

booleanOperations :: [[ Operator Parser Expr ]]
booleanOperations =
  [
    [ Prefix (Not <$ symbol "!") ]
  ,
    [ InfixR (And <$ symbol "&&")
    , InfixR (Or <$ symbol "||")
    ]
  ]

booleanExpr :: Parser Expr
booleanExpr = makeExprParser booleanTerm booleanOperations
