-- AOP 6.5
{-# LANGUAGE LambdaCase #-}
module Nat where

import Prelude hiding (succ, last)

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

-------------------------------
-- Coreflexive morphisms
-- These are all restricted to
-- Nat, without type signature
-------------------------------

-- successor
succ = Succ
-- pred
succ' (Succ n) = n

-- coreflexive over (Succ n) but Zero
positive = succ . succ'

-- Y for Nat
fixN f = \case
  Zero     -> Zero
  (Succ n) -> Succ (f n)

-- I for Nat
idN = fixN idN

-- coreflexive over Zero but (Succ n)
corefZero = foldn (Zero, positive)
-- the converse of corefZero equals corefZero,
-- because corefZero can be represented by [(Zero, Zero)] on REL.
corefZero' = corefZero

-- const Zero
constZero = constN Zero

-- coreflexive over n but under n
corefGreaterThan = foldn (positive, (succ.).(.succ'))

-- coreflexive less than n but over Succ n
corefLessEqual = foldn (corefZero, fixN)

-- equal corefZero
zero = corefZero --  constZero . corefZero'

-- const n
constN n = foldn (n, idN)

-- coref one
one n@(Succ Zero) = n
-- converse of one
one' = one

-- generate coref only n value
corefOnly = foldn (zero, (succ .).(. succ'))

-- binary relation operator equal
eq = corefOnly

-- coreflexive between lb+1 to ub
-- range x y == (x, y]
range lb ub = corefLessEqual ub . corefGreaterThan lb

-- exchange Zeron and n
exchangeZero n Zero = n
exchangeZero n m = if n == m then Zero else m
-- exchange n and Zero which is converse of exchangeZero
exchangeZero' = exchangeZero

exchange n m = exchangeZero' n . exchangeZero m . exchangeZero n

-- sample
exchZeroOne = exchange Zero (Succ Zero) 
exchZeroOne' = exchZeroOne
excludeOne = exchZeroOne' . positive . exchZeroOne

toZero n m = if n == m then Zero else m

-- coreflexive just only 2,4,5,7,8,9(3 and 6 are bottom)
sample245789 = ex' . rg . ex
  where
    rg = range (toNat 3) (toNat 9)
    ex = exchange (toNat 2) (toNat 6)
    ex' = exchange (toNat 6) (toNat 2)

sample348over = down' . rg . down
  where
    rg = corefGreaterThan (Succ (Succ Zero))
    down = toZero (toNat 5) . toZero (toNat 6) . toZero (toNat 7)
    down' = down

corefEven Zero = Zero
corefEven (Succ n) = Succ (corefOdd n)

corefOdd (Succ n) = Succ (corefEven n)

outl (x, y) = x
outr (x, y) = y
sqr = outl . foldn ((0, 0), h)
  where h (s, dn) = (s + dn + 1, dn + 2)

last p = outl . foldn ((0, 0), h)
  where h (pn, n) | p (n+1)   = (n+1, n+1)
                  | otherwise = (pn,  n+1)

-- |             [zero,succ]
--         N <---------------- 1 + N
--        /|                     |
--       / |                     |
--      /  |                     |
--     /   | <fib,fib'>          | 1 + <fib,fib'>
--    /    |                     |
--   /     |                     |
--  v      v                     v
-- F(N)<-F(N)xF'(N) <------ 1 + F(N)xF'(N)
--                    [c,f]
-- where
--   fib' n = fib (n+1)
--   F(N) = F(N+1)
fib = outl . foldn (c, f)
  where
    c = (0, 1)
    f (x, y) = (y, x + y)

ack (Zero, y) = Succ y
ack (Succ x, Zero) = ack (x, Succ Zero)
ack (Succ x, Succ y) = ack (x, ack (Succ x, y))

ack' = foldn (Succ, f)
  where f g = foldn (g (Succ Zero), g)

ack'' = foldn (Succ, swap f)
  where f = foldn (ap1, ap2)
        ap1 g = g (Succ Zero)
        ap2 g h = h (g h)
        swap f a b = f b a
