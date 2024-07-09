{-# LANGUAGE Haskell2010 #-}
module Interp where

import Prelude hiding (lookup)

type Name = String
data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | At Position Term

type Position = (Int, Int)

data Value = Wrong
           | Num Int
           | Fun (Value -> P Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<fun>"

interp :: Term -> Environment -> P Value
interp (Var x) e = lookup x e
interp (Con i) e = unitP (Num i)
interp (Add u v) e =
  interp u e `bindP` (\a -> interp v e `bindP` (\b -> add a b))
interp (Lam x v) e = unitP (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e =
  interp t e `bindP` (\f -> interp u e `bindP` (\a -> apply f a))
interp (At p t) e = resetP p (interp t e)
{--
lookup :: Name -> Environment -> M Value
lookup x [] = unitM Wrong
lookup x ((y,b):e) = if x == y then unitM b else lookup x e
--}
lookup :: Name -> Environment -> P Value
lookup x [] = errorP ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x == y then unitP b else lookup x e
{--
add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a       b       = unitM Wrong
--}
add :: Value -> Value -> P Value
add (Num i) (Num j) = unitP (Num (i+j))
add a       b       = errorP $ "should be numbers: " ++ showval a ++ ", " ++ showval b

{--
apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f       a = unitM Wrong
--}
apply :: Value -> Value -> P Value
apply (Fun k) a = k a
apply f       _ = errorP $ "should be function: " ++ showval f

test :: Term -> String
test t = showP (interp t [])

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11))

term1 :: Term
term1 = At (0,1) (App (At (0,2) (Lam "x" (At (1,1) (Add (At (1,2) (Var "x")) (At (2,2) (Var "x"))))))
                   (At (0,3) (App (At (2,1) (Con 10)) (At (2,2) (Con 11)))))

{--
type M a = a
unitM :: a -> M a
unitM a = a
bindM :: M a -> (a -> M b) -> M b
a `bindM` k = k a
showM :: M Value -> String
showM a = showval a
--}

data E a = Success a | Error String
unitE a  = Success a
errorE s = Error s
(Success a) `bindE` k = k a
(Error s) `bindE` k = Error s
showE (Success a) = "Success: " ++ showval a
showE (Error s)   = "Error: " ++ s

type P a = Position -> E a

unitP :: a -> P a
unitP a = \p -> unitE a
errorP :: String -> P a
errorP s = \p -> errorE $ (showpos p ++  ": " ++ s)

showpos :: Position -> String
showpos p = show p

bindP :: P a -> (a -> P b) -> P b
m `bindP` k = \p -> m p `bindE` (\x -> k x p)

resetP :: Position -> P a -> P a
resetP q m = \p -> m q

showP :: P Value -> String
showP m = showE (m (0, 0))
