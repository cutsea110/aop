module Chap03 where

import Prelude hiding (foldr, sum, length, div)

-- | Ex 3.1
--
-- FX = 1 + N * X
--
-- <[zero, plus] . Foutl, [zero, succ . outr] . Foutr> = <zeross, pluss> を示す.
--
foldr (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

sum = foldr (zero, plus)
  where zero = 0
        plus (x, y) = x + y

length = foldr (zero, succ . outr)
  where zero = 0
        outr (x, y) = y
        succ n = 1 + n

average = div . pair (sum, length)
  where
    pair (f, g) x = (f x, g x)
    div (x, y) = x / y

avg = div . foldr (zeross, pluss)
      where zeross = (0, 0)
            pluss (a, (b, n)) = (a + b, n + 1)
            div (x, y) = x / y
