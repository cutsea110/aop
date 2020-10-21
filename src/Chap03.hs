{-# LANGUAGE NPlusKPatterns #-}
module Chap03 where

import Prelude hiding (foldr, sum, product, length, div)

-- | Ex 3.1
--
-- FX = 1 + N * X
--
-- <[zero, plus] . Foutl, [zero, succ . outr] . Foutr> = [zeross, pluss] を示す.
--
foldr (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

sum :: Num a => [a] -> a
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
-- | Ex 3.3
--
naiveSteep [] = True
naiveSteep (a:x) = a > sum x && naiveSteep x

--            [nil, cons]
--         TB <-------- 1 + B * TB
--        /|                |
--       / |                |
-- steep/  |                |
--     /   |(|c,f|)         |1 + 1 * <steep, sum>
--    /    | = <steep,sum>  |
--   /     |                |
--  v      |                |
--  A <- A * B <-------- 1 + B * (A * B)
--    outl        [c, f]
--
steep = outl . foldr (c, f)
  where outl (x, _) = x
        c = (True, 0)
        f (a, (b, x)) = (a > x && b, a + x)

test_naiveSteep = naiveSteep $ map (2^) ([5000,4999..0] :: [Integer])
test_steep = steep $ map (2^) ([5000,4999..0] :: [Integer])

-- | Ex 3.4
--
--         a
--   T <------- FT
--   |           |
--  f|           |F<f, (|h|)>
--   |           |
--   v           v
--   A <------- F(A * B)
--         g
--
--                           a
--            T <------------------------- FT
--           /|\                           /|\
--          / | \                         / | \
--         /  |  \                       /  |  \
--       f/   |   \                   Ff/   |   \
--       /  (|k|)  \ (|h|)             /  F(|k|) \F(|h|)
--      /     =     \                 /     =     \
--     /   <f,(|h|)> \               /  F<f,(|h|)> \
--    v       v       v             v       v       v
--   A <--- A * B ---> B          FA <- F(A * B) -> FB
--   ^  outl  ^        ^            Foutl  /  Foutr /
--    \        \        \__________________________/
--     \        \                   h    /
--      \        \_____________________ /
--       \                 k           /
--        \___________________________/
--                       g
--
-- k の定義
--   k : A * B <- F(A * B)
--   k = <g, h . Foutr>
--
-- cata の普遍性から
-- (|k|) = <f, (|h|)> == <f, (|h|)> . a = k . F<f, (|h|)>
--
--  k . F<f, (|h|)>
-- == {- k の定義 -}
--  <g, h . Foutr> . F<f, (|h|)>
-- == {- 対の融合 -}
--  <g . F<f, (|h|)>, h . Foutr . F<f, (|h|)>>>
-- == {- 関手則および対の消去則 -}
--  <g . F<f, (|h|)>, h . F(|h|)>
-- == {- 所与の図式および cata -}
--  <f . a, (|h|) . a>
-- == {- 対の融合 -}
--  <f, (|h|)> . a
--
-- | Ex 3.5
--
data Tree a = Null
            | Node (Tree a, a, Tree a)
            deriving Show
--
-- balanced
--    x 1/3 <= n/(n+m+1) <= 2/3
--    o n + 1 <= 3(m + 1) /\ m + 1 <= 3(n + 1)
--
--       [null, node]
--  Ta <------------- 1 + Ta * a * Ta
--  |                        |
-- u|                        | 1 + u * 1 * u
--  v                        v
--  X  <------------- 1 + X  * a * X
--         [c, f]

foldt :: (a, (a, b, a) -> a) -> Tree b -> a
foldt (c, f) = u
  where u Null = c
        u (Node (l, x, r)) = f (u l, x, u r)

size :: Fractional b => Tree a -> b
size = foldt (c, f)
  where c = 0
        f (n, a, m) = n + 1 + m

naiveBalanced :: Tree a -> Bool
naiveBalanced Null = True
naiveBalanced (Node (x, a, y)) = balanced x && balanced y &&
                            n + 1 <= 3 * (m + 1) && m + 1 <= 3 * (n + 1)
  where (n, m) = (size x, size y)

balanced :: Tree a -> Bool
balanced = outl . foldt (c, f)
  where outl (x, _) = x
        c = (True, 0)
        f ((b, n), a, (c, m)) = (b && c && n + 1 <= 3 * (m + 1) && m + 1 <= 3 * (n + 1), n + 1 + m)

-- | Ex 3.6
--
foldn (c, f) 0 = c
foldn (c, f) (n+1) = f (foldn (c, f) n)

preds = outl . foldn (c, f)
  where outl (x, _) = x
        c = ([], 0)
        f (ns, n) = (n+1:ns, n+1)

-- | Ex 3.7
--
product = foldr (c, f)
  where c = 1
        f (n, m) = n * m

-- fact = product . preds
fact = outl . foldn (c, f)
  where outl (x, _) = x
        c = (1, 0)
        f (m, n) = ((n+1)*m, n+1)
