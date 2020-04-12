{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Subfactorial where

pair (f, g) x = (f x, g x)
outl (x, _) = x
outr (_, y) = y

type Nat = Integer

foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) = u
  where u 0     = c
        u (n+1) = f (u n)

paran :: (a, Nat -> a -> a) -> Nat -> a
paran (c, f) = u
  where u 0 = c
        u (n+1) = f (n+1) (u n)

-- >>> fact 9
-- 362880
fact = paran (1, (*))

-- >>> subfact 9
-- 133496
subfact = snd . paran (c, f)
  where c = (0, 1)
        f i (x, y) = (y, (i-1)*(x+y))

-- using barbed wire split law (like banana split law)
fact'subfact = pair (outl, outr . outr) . paran (c, f)
  where c = (1, (0, 1))
        f n (x, (y, z)) = (n * x, (z, (n-1)*(y+z)))

-- >>> calc 9
-- 2.71828369389345
-- >>> calc 20
-- 2.718281828459045
-- >>> exp 1
-- 2.718281828459045
calc n = fromIntegral num / fromIntegral den
  where (num, den) = fact'subfact n

test n = calc n == exp 1
