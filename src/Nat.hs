module Nat where

import Prelude hiding (succ, const)

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
    

para (c, g) Zero = c
para (c, g) (Succ n) = g (para (c, g) n, n)

plus x = foldn (x, Succ)
mult x = foldn (Zero, plus x)
expr x = foldn (Succ Zero, mult x)
fact = para (Succ Zero, f)
  where
    f (m, n) = mult m (Succ n)

toNat = unfoldn (\n -> if n <= 0 then Nothing else Just (n-1))
fromNat = foldn (0, (1+))

succ = Succ
succ' (Succ n) = n

-- coreflexive over (Succ n) but Zero
positive = succ . succ'

-- coreflexive over Zero but (Succ n)
corefZero = foldn (Zero, positive)

-- const Zero
constZero = foldn (Zero, id)

-- the converse of corefZero equals corefZero,
-- because corefZero can be represented by [(Zero, Zero)] on REL.
corefZero' = corefZero

zero :: Nat -> Nat
zero = constZero . corefZero'

-- const n
const n = foldn (n, id)

-- coref one
one n@(Succ Zero) = n
one' = one
