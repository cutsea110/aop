{-# LANGUAGE NPlusKPatterns #-}
module Ack where

cata :: (a, a -> a) -> Int -> a
cata (c, f) 0 = c
cata (c, f) (n+1) = f (cata (c, f) n)

ack (0, y) = y + 1
ack (x+1, 0) = ack (x, 1)
ack (x+1, y+1) = ack (x, ack (x+1, y))

ack' = cata (succ, f)
  where f g = cata (g 1, g)

ack'' = cata (succ, swap f)
  where
    f = cata (ap1, ap2)
    ap1 g = g 1
    ap2 g h = h (g h)
    swap f a b = f b a
