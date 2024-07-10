{-# LANGUAGE Haskell2010 #-}
module Interp where

import Prelude hiding (lookup)

type Name = String

{- | Variation 0: Identity Monad -}
data TermI = VarI Name
           | ConI Int
           | AddI TermI TermI
           | LamI Name TermI
           | AppI TermI TermI

data ValueI = WrongI
            | NumI Int
            | FunI (ValueI -> I ValueI)

type Environment = [(Name, ValueI)]

showvalI :: ValueI -> String
showvalI WrongI = "<wrong>"
showvalI (NumI i) = show i
showvalI (FunI _) = "<function>"

interpI :: TermI -> Environment -> I ValueI
interpI (VarI x) e = lookupI x e
interpI (ConI i) _ = unitI (NumI i)
interpI (AddI u v) e = interpI u e `bindI` (\a ->
                       interpI v e `bindI` (\b ->
                       addI a b))
interpI (LamI x v) e = unitI (FunI (\a -> interpI v ((x, a) : e)))
interpI (AppI t u) e = interpI t e `bindI` (\f ->
                       interpI u e `bindI` (\a ->
                       applyI f a))

lookupI :: Name -> Environment -> I ValueI
lookupI x [] = unitI WrongI
lookupI x ((y, b) : e)
  | x == y    = unitI b
  | otherwise = lookupI x e

addI :: ValueI -> ValueI -> I ValueI
addI (NumI i) (NumI j) = unitI (NumI (i + j))
addI _        _        = unitI WrongI

applyI :: ValueI -> ValueI -> I ValueI
applyI (FunI k) a = k a
applyI _        _ = unitI WrongI

testI :: TermI -> String
testI t = showI (interpI t [])

type I a = a
unitI a = a
a `bindI` k = k a
showI a = showvalI a

term0I :: TermI
term0I = AppI (LamI "x" (AddI (VarI "x") (VarI "x"))) (AddI (ConI 10) (ConI 11))
