module Expressions
  ( Expr (Compose), Atom (Var, Con)
  , VarName, ConName, deCompose, expr
  ) where

import Data.Char (isAlphaNum, isAlpha, isDigit)
import Data.List (intersperse)
import Parsing
import Utilities (compose)

newtype Expr = Compose [Atom] deriving Eq
data Atom = Var VarName | Con ConName [Expr] deriving Eq
type VarName = String
type ConName = String

expr :: Parser Expr
expr = simple >>= rest
  where
    rest s1 = do { op <- operator
                 ; s2 <- simple
                 ; return (Compose [Con op [s1, s2]])
                 }
              <|> return s1


operator :: Parser String
operator = do { op <- token (some (sat symbolic))
              ; Parsing.guard (op /= "." && op /= "=")
              ; return op
              }

symbolic = (`elem` opsymbols)
opsymbols = "!@#$%*&+./<=>?\\^|:-~"

simple :: Parser Expr
simple = do { es <- somewith (symbol ".") term
            ; return (Compose (concatMap deCompose es))
            }

deCompose :: Expr -> [Atom]
deCompose (Compose as) = as

term :: Parser Expr
term = ident args <|> paren expr
args = many (ident none <|> paren expr)

ident :: Parser [Expr] -> Parser Expr
ident args = do { x <- token (some (sat isAlphaNum))
                ; Parsing.guard (isAlpha (head x))
                ; if isVar x
                  then return (Compose [Var x])
                  else if (x == "id")
                       then return (Compose [])
                       else do { as <- args
                               ; return (Compose [Con x as])
                               }
                }

isVar [x] = True
isVar [x, d] = isDigit d
isVar _ = False

instance Show Expr where
  showsPrec p (Compose []) = showString "id"
  showsPrec p (Compose [a]) = showsPrec p a
  showsPrec p (Compose as) = showParen (p > 0) (showSep " . " (showsPrec 1) as)

showSep :: String -> (a -> ShowS) -> [a] -> ShowS
showSep sep f = compose . intersperse (showString sep) . map f

showSpace = showChar ' '

instance Show Atom where
  showsPrec p (Var v) = showString v
  showsPrec p (Con f []) = showString f
  showsPrec p (Con f [e1, e2])
    | isOp f = showParen (p > 0) (showsPrec 1 e1 . showSpace . showString f . showSpace . showsPrec 1 e2)
  showsPrec p (Con f es) = showParen (p > 1) (showString f . showSpace . showSep " " (showsPrec 2) es)

isOp f = all symbolic f
