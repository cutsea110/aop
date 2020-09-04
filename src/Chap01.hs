{-# LANGUAGE NPlusKPatterns #-}
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

-- | Ex 1.6
foldn' :: (a, a -> a) -> Int -> a
foldn' (c, f) = u
  where u 0 = c
        u (n+1) = f (u n)

{--
cack :: Nat -> Nat -> Nat
cack = foldn (Succ, swap f)
  where f = foldn (ap1, ap2)
        ap1 g = g (Succ Zero)
        ap2 g h = h (g h)
        swap f a b = f b a
--}

{--
-- step 0
ack (0, y) = y + 1
ack (x+1, 0) = ack (x, 1)
ack (x+1, y+1) = ack (x, ack (x+1, y))
--}

{--
-- step 1
cack 0 y = y + 1
cack (x+1) 0 = cack x 1
cack (x+1) (y+1) = cack x (cack (x+1) y)
--}

{--
-- step 2
f y = y + 1       -- f = cack 0
g 0 = h 1         -- g = cack (x+1), h = cack x
g (y+1) = h (g y) -- same as above
--}

{--
-- step 3
f = (+1)
g = foldn' (h 1, h)
--}


{--
-- step 4
cack 0 = (+1)
cack (x+1) = foldn' (cack x 1, cack x)
--}

{--
-- step 5
cack 0 = (+1)
cack (x+1) = k (cack x)
  where k v = foldn' (v 1, v)
--}

-- step 6
cack = foldn' ((+1), k)
  where k v = foldn' (v 1, v)

ack (0, y) = y + 1
ack (x+1, 0) = ack (x, 1)
ack (x+1, y+1) = ack (x, ack (x+1, y))
