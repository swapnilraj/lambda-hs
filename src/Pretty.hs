module Pretty where

import AST
import Data.Text.Prettyprint.Doc

instance Pretty Expr where
  pretty (Var n) = pretty n
  pretty (Lit (LInt i)) = pretty i
  pretty (Lit (LBool b)) = pretty b
  pretty (Abs arg body) = backslash <> pretty arg <> dot <> pretty body
  pretty (App f g) = parens (pretty f) <+> pretty g
  pretty (Negation f) = pretty "-" <+> pretty f
  pretty (Sum x y) = pretty x <+> pretty "+" <+> pretty y
  pretty (Substract x y) = pretty x <+> pretty "-" <+> pretty y
  pretty (Product x y) = pretty x <+> pretty "*" <+> pretty y
  pretty (Division x y) = pretty x <> slash <> pretty y
  pretty (And x y) = pretty x <+> pretty "and" <+> pretty y
  pretty (Or x y) = pretty x <+> pretty "or" <+> pretty y
  pretty (Not x) = pretty "not" <+> pretty x
  pretty (Less x y) = pretty x <+> langle <+> pretty y
  pretty (Greater x y) = pretty x <+> rangle <+> pretty y
  pretty (Equal x y) = pretty x <+> equals <+> pretty y
