module Chap03 where

import Prelude hiding (foldr, sum, length, div)

-- | Ex 3.1
--
-- FX = 1 + N * X
--
-- <[zero, plus] . Foutl, [zero, succ . outr] . Foutr> = [zeross, pluss] を示す.
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
--
--  <[zero, plus] . Foutl, [zero, succ . outr] . Foutr>
-- ==
--  <[zero, plus] . (id + id * outl), [zero, succ . outr] . (id + id * outr)>
-- ==
--  <[zero, plus . (id, outl)], [zero, succ . outr . (id * outr)]>
-- ==
--  [<zero, zero>, <plus . (id * outl), succ . outr . (id * outr)>]
--
-- 第2成分をポイントワイズに解くと
--  <plus . (id * outl), succ . outr . (id * outr)> (a, (b, n))
-- ==
--  (plus . (id * outl) (a, (b, n)), succ . outr . (id * outr) (a, (b, n)))
-- ==
--  (plus (a, b), succ (outr (a, n)))
-- ==
--  (a+b, 1+n)
--
-- | Ex 3.2
--
-- <Foutl, Foutr> : FA * FB <- F(A * B)
--
--                 <Foutl, Foutr>
--      FA * FB <----------------- F(A * B)
--         |                           |
-- Fh * Fk |                           |  F(h * k)
--         |                           |
--         v                           v
--      FC * FD <----------------- F(C * D)
--                 <Foutl, Foutr>
--
--
--  (Fh * Fk) . <Foutl, Foutr>
-- == {- 積の吸収則 (2.8) -}
--  <Fh . Foutl, Fk . Foutr>
-- == {- 下図 -}
--  <Foutl . F(h * k), Foutr . F(h * k)>
-- == {- 積の融合則 (2.6) -}
--  <Foutl, Foutr> . F(h * k)
--
--  Fh . Foutl = Foutl . F(h * k)   Fk . Foutr = Foutr . F(h * k)
--
--       Foutl                         Foutr
--   FA <----- F(A * B)            FB <----- F(A * B)
--    |            |               |             |
--  Fh|            | F(h * k)    Fk|             | F(h * k)
--    v            v               v             v
--   FC <----- F(C * D)            FD <----- F(C * D)
--       Foutl                         Foutr
--
