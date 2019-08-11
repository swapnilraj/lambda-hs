module Eval where

import AST
import Data.Map as Map
import Pretty

type Scope = Map Name Value

data Value
  = VInt Int
  | VBool Bool
  | VClosure Scope Name Expr
  deriving (Eq)

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VClosure _ _ e) = show e

apply :: Value -> Value -> Value
apply (VClosure env name clo) exp = eval (insert name exp env) clo
apply _ _ = error "Cannot apply a non-closure"

eval :: Scope -> Expr -> Value
eval env expr
  | (Var v) <- expr              = env ! v
  | (Lit (LInt i)) <- expr       = VInt i
  | (Lit (LBool b)) <- expr      = VBool b
  | (Abs (Var arg) body) <- expr = VClosure env arg body
  | (App f g) <- expr            = let f' = eval env f
                                       g' = eval env g
                                   in apply f' g'
  | (Sum f g) <- expr            = let (VInt f') = eval env f
                                       (VInt g') = eval env g
                                   in VInt $ f' + g'
  | (Substract f g) <- expr      = let (VInt f') = eval env f
                                       (VInt g') = eval env g
                                   in VInt $ f' - g'
  | (Product f g) <- expr        = let (VInt f') = eval env f
                                       (VInt g') = eval env g
                                   in VInt $ f' * g'
  | (Division f g) <- expr       = let (VInt f') = eval env f
                                       (VInt g') = eval env g
                                   in VInt $ quot f' g'
  | (Negation f) <- expr         = let (VInt f') = eval env f
                                   in VInt $ negate f'
  | (And f g) <- expr            = let (VBool f') = eval env f
                                       (VBool g') = eval env g
                                   in VBool $ f' && g'
  | (Or f g) <- expr             = let (VBool f') = eval env f
                                       (VBool g') = eval env g
                                   in VBool $ f' || g'
  | (Not f) <- expr              = let (VBool f') = eval env f
                                   in VBool $ not f'
  | (Less f g) <- expr           = let (VInt f') = eval env f
                                       (VInt g') = eval env g
                                    in VBool $ f' < g'
  | (Greater f g) <- expr        = let (VInt f') = eval env f
                                       (VInt g') = eval env g
                                   in VBool $ f' > g'
  | (Equal f g) <- expr          = let f' = eval env f
                                       g' = eval env g
                                   in VBool $ f' == g'

runEval :: Expr -> Value
runEval = eval empty
