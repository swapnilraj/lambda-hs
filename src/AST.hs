module AST where

type Name = String

data Expr =
  Var Name
  | App Expr Expr
  | Abs Expr Expr
  | Lit Int
  deriving (Show)
