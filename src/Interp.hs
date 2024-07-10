{-# LANGUAGE Haskell2010 #-}
module Interp where

import Prelude hiding (lookup)

type Name = String

{- | Variation 0: Standard Interpreter -}
data TermI = VarI Name
           | ConI Int
           | AddI TermI TermI
           | LamI Name TermI
           | AppI TermI TermI

data ValueI = WrongI
            | NumI Int
            | FunI (ValueI -> I ValueI)

type EnvironmentI = [(Name, ValueI)]

showvalI :: ValueI -> String
showvalI WrongI = "<wrong>"
showvalI (NumI i) = show i
showvalI (FunI _) = "<function>"

interpI :: TermI -> EnvironmentI -> I ValueI
interpI (VarI x) e = lookupI x e
interpI (ConI i) _ = unitI (NumI i)
interpI (AddI u v) e = interpI u e `bindI` (\a ->
                       interpI v e `bindI` (\b ->
                       addI a b))
interpI (LamI x v) e = unitI (FunI (\a -> interpI v ((x, a) : e)))
interpI (AppI t u) e = interpI t e `bindI` (\f ->
                       interpI u e `bindI` (\a ->
                       applyI f a))

lookupI :: Name -> EnvironmentI -> I ValueI
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


{- | Variation 1: Error Message -}
data TermE = VarE Name
           | ConE Int
           | AddE TermE TermE
           | LamE Name TermE
           | AppE TermE TermE

data ValueE = WrongE
            | NumE Int
            | FunE (ValueE -> E ValueE)

type EnvironmentE = [(Name, ValueE)]

showvalE :: ValueE -> String
showvalE WrongE = "<wrong>"
showvalE (NumE i) = show i
showvalE (FunE _) = "<function>"

interpE :: TermE -> EnvironmentE -> E ValueE
interpE (VarE x) e = lookupE x e
interpE (ConE i) _ = unitE (NumE i)
interpE (AddE u v) e = interpE u e `bindE` (\a ->
                       interpE v e `bindE` (\b ->
                       addE a b))
interpE (LamE x v) e = unitE (FunE (\a -> interpE v ((x, a) : e)))
interpE (AppE t u) e = interpE t e `bindE` (\f ->
                       interpE u e `bindE` (\a ->
                       applyE f a))

lookupE :: Name -> EnvironmentE -> E ValueE
lookupE x [] = errorE ("unbound variable: " ++ x)
lookupE x ((y, b) : e)
  | x == y    = unitE b
  | otherwise = lookupE x e

addE :: ValueE -> ValueE -> E ValueE
addE (NumE i) (NumE j) = unitE (NumE (i + j))
addE a        b        = errorE $ "should be numbers: " ++ showvalE a ++ ", " ++ showvalE b

applyE :: ValueE -> ValueE -> E ValueE
applyE (FunE k) a = k a
applyE f        _ = errorE $ "should be function: " ++ showvalE f

testE :: TermE -> String
testE t = showE (interpE t [])

data E a = Success a | Error String
unitE a = Success a
errorE s = Error s
(Success a) `bindE` k = k a
(Error s)   `bindE` k = Error s
showE (Success a) = "Success: " ++ showvalE a
showE (Error s)   = "Error: " ++ s

term0E :: TermE
term0E = AppE (LamE "x" (AddE (VarE "x") (VarE "x"))) (AddE (ConE 10) (ConE 11))
term1E :: TermE -- Error: should be function: 1
term1E = AppE (ConE 1) (ConE 2)
