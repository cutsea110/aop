{-# LANGUAGE Haskell2010 #-}
module Interp where

import Prelude hiding (lookup)

type Name = String
data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> I Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<fun>"

interp :: Term -> Environment -> I Value
interp (Var x) e = lookup x e
interp (Con i) e = unitI (Num i)
interp (Add u v) e =
  interp u e `bindI` (\a -> interp v e `bindI` (\b -> add a b))
interp (Lam x v) e = unitI (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e =
  interp t e `bindI` (\f -> interp u e `bindI` (\a -> apply f a))

lookup :: Name -> Environment -> I Value
lookup x [] = unitI Wrong
lookup x ((y,b):e) = if x == y then unitI b else lookup x e

add :: Value -> Value -> I Value
add (Num i) (Num j) = unitI (Num (i+j))
add a       b       = unitI Wrong

apply :: Value -> Value -> I Value
apply (Fun k) a = k a
apply f       a = unitI Wrong

test :: Term -> String
test t = showI (interp t [])

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11))

type I a = a
unitI :: a -> I a
unitI a = a
bindI :: I a -> (a -> I b) -> I b
a `bindI` k = k a
showI :: I Value -> String
showI a = showval a

