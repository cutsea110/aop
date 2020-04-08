{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Subfactorial where

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
subfact 1 = 0
subfact 2 = 1
subfact n = snd $ paran ((0, 1), \i (x, y) -> (y, (i-1)*(x+y))) n

-- >>> e 9
-- 2.71828369389345
-- >>> e 20
-- 2.718281828459045
-- >>> exp 1
-- 2.718281828459045
e n = fromIntegral (fact n) / fromIntegral (subfact n)

test n = exp 1 == e n

