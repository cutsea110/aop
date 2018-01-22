-- AOP 6.5
module Nat where

import Prelude hiding (succ)

data Nat = Zero
         | Succ Nat
         deriving (Show, Eq)

foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) Zero = c
foldn (c, f) (Succ n) = f (foldn (c, f) n)

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn phi x = case phi x of
  Nothing -> Zero
  Just x' -> Succ (unfoldn phi x')

hylo (c, f, g, p) a = case p a of
  True  -> c
  False -> f (b, hylo (c, f, g, p) a')
    where
      (b, a') = g a

fact' = hylo (Succ Zero, f, g, p)
  where
    p n = n == Zero
    g (Succ n) = (Succ n, n)
    f = uncurry mult
    

paran (c, g) Zero = c
paran (c, g) (Succ n) = g (paran (c, g) n, n)

plus x = foldn (x, Succ)
mult x = foldn (Zero, plus x)
expr x = foldn (Succ Zero, mult x)
fact = paran (Succ Zero, f)
  where
    f (m, n) = mult m (Succ n)

toNat = unfoldn (\n -> if n <= 0 then Nothing else Just (n-1))
fromNat = foldn (0, (1+))

succ = Succ
succ' (Succ n) = n

-- coreflexive over (Succ n) but Zero
positive = succ . succ'

-- coreflexive over n but under n
corefGreaterThan = foldn (positive, (succ.).(.succ'))

-- coreflexive over Zero but (Succ n)
corefZero = foldn (Zero, positive)

-- const Zero
constZero = foldn (Zero, id)

-- the converse of corefZero equals corefZero,
-- because corefZero can be represented by [(Zero, Zero)] on REL.
corefZero' = corefZero

zero :: Nat -> Nat
zero = corefZero --  constZero . corefZero'

-- const n
constN n = foldn (n, id)

-- coref one
one n@(Succ Zero) = n
one' = one

-- generate coref only n value
corefOnly = foldn (zero, (succ .).(. succ'))

-- binary relation operator equal
eq = corefOnly

-- coreflexive less than n but over Succ n
corefLessEqual = foldn (zero, sub)
  where
    sub f = paran (Zero, g f)
    g h (x, y) = const (Succ (h y)) x

-- coreflexive between lb+1 to ub
-- range x y == (x, y]
range lb ub = corefLessEqual ub . corefGreaterThan lb

exchZeroOne Zero = Succ Zero
exchZeroOne (Succ Zero) = Zero
exchZeroOne n = n

exchZeroOne' = exchZeroOne

excludeOne = exchZeroOne' . positive . exchZeroOne

exchangeZero n Zero = n
exchangeZero n m = if n == m then Zero else m
exchangeZero' = exchangeZero

exchange n m = exchangeZero' n . exchangeZero m . exchangeZero n

-- coreflexive just only 2,4,5,7,8,9(3 and 6 are bottom)
sample245789 = ex' . rg . ex
  where
    rg = range (toNat 3) (toNat 9)
    ex = exchange (toNat 2) (toNat 6)
    ex' = exchange (toNat 6) (toNat 2)

sample348over = ex' . rg . ex
  where
    rg = corefGreaterThan (Succ (Succ Zero))
    ex (Succ (Succ (Succ (Succ (Succ Zero))))) = Zero
    ex (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))) = Succ Zero
    ex (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))) = Succ (Succ Zero)
    ex n = n
    ex' = ex

