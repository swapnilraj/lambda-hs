module AST where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Abs Expr Expr
  | Lit Lit
  | Negation Expr
  | Sum Expr Expr
  | Substract Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Less Expr Expr
  | Greater Expr Expr
  | Equal Expr Expr
  deriving (Eq)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq)
