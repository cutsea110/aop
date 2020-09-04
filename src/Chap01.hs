module Chap01 where

import Prelude hiding (last)

-- | Ex 1.1
-- solution 1
k1 x = if k1 x == 0 then 1 else 0

-- solution 2
k2 x = not (k2 x)

-- solution 3
k3 x = k3 x + 1

-- solution 4
k4 x = k4 x ++ [x]

-- | Ex 1.2
m (x, y) = if x == y
           then y + 1
           else m (x, m (x - 1, y + 1))
-- solution 1
m1 (x, y) = x + 1

-- solution 2
m2 (x, y) = if x >= y
            then x + 1
            else y - 1

-- | Ex 1.3

data Nat = Zero
         | Succ Nat
         deriving (Show, Eq)

foldn (c, f) = u
  where u Zero     = c
        u (Succ n) = f (u n)

data NatPlus = One
             | Next NatPlus
         deriving (Show, Eq)

foldnplus (c, f) = u
  where u One      = c
        u (Next n) = f (u n)

f :: NatPlus -> Nat
f = foldnplus (Zero, Succ)

g :: Nat -> NatPlus
g = foldn (One, Next)

-- test
testfgIsId n = n == (f . g) n
testgfIsId n = n == (g . f) n

-- | Ex 1.4
plus = foldn (id, (Succ .))
outl (x, _) = x
sqr = f . foldn (c, h)
  where f = outl
        c = (zero, zero)
        h (s, dn) = (plus (plus s dn) one, plus dn two)
        (zero, one, two) = (Zero, Succ zero, Succ one)

-- | Ex 1.5
last :: (Nat  -> Bool) -> Nat -> Nat
last p = f . foldn (c, h)
  where f = outl
        c = (Zero, Succ Zero)
        h (l, n) = if p n then (n, Succ n) else (l, Succ n)

test_1_5 = map (fromNat . last evenp . toNat) [0..100]
  where
    evenp, oddp :: Nat -> Bool
    evenp Zero = True
    evenp (Succ n) = oddp n
    oddp Zero = False
    oddp (Succ n) = evenp n

    toNat 0 = Zero
    toNat n = Succ (toNat (n-1))
    fromNat Zero = 0
    fromNat (Succ n) = 1 + fromNat n
