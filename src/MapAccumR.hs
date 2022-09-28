{-# LANGUAGE TupleSections  #-}
module MapAccumR where

import Control.Arrow ((***), (&&&), first, second)

-- |
--                   [nil, cons]
--           Ta <----------------- 1 + A x Ta
--           |                       |
--  (|c, f|) |                       | id + ida x (|c,f|)
--           v                       v
--           X  <----------------- 1 + A x X
--                   [c, f]
--
cata :: (b, (a, b) -> b) -> [a] -> b
cata (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

-- |
--                            [nil, cons]                                    *   x   xs
--  Ta <-------------------------------------------------------------------- 1 + A * Ta
--   |                                                                            |
--   |                                                                            |
-- u |                                                                            | id + ida * u
--   |                                                                            |
--   v    cons                assocr                 f              assocl+swap   v
-- X * Tb <-- 1 + X * (B * Tb) <-- 1 + (X * B) * Tb <-- 1 + (X * A) * Tb <-- 1 + A * (X * Tb)
--                                                                           *   x    u xs
--
--
mapAccumR :: (x, (x, a) -> (x, b)) -> [a] -> (x, [b])
mapAccumR (s, f) = cata ((s,[]), second cons . assocr . first (f . swap) . assocl)
{--
mapAccumR f s = u
  where u [] = (s, [])
        u (x:xs) = (second (uncurry (:)) . assocr . first (f . swap) . assocl) (x, u xs)
--}
cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs
assocl ::  (a, (b, c)) -> ((a, b), c)
assocl (a, (x, y)) = ((a, x), y)
assocr :: ((a, b), c) -> (a, (b, c))
assocr ((x, b), y) = (x, (b, y))
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
