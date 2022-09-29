{-# LANGUAGE TupleSections  #-}
module MapAccumR where

import Control.Arrow ((***), (&&&), first, second)
import Curry (phi)

-- | cata for list
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

-- | ana for snoc list
--                   alpha
--           Tb <----------------- 1 + Tb x B
--           ^                       ^
--   [(psi)] |                       | id + [(psi)] x ida
--           |                       |
--           X  -----------------> 1 + X x B
--                    psi
--
ana :: (a -> Maybe (a, b)) -> a -> [b]
ana psi = v
  where v x = case psi x of
          Nothing     -> []
          Just (xs, x) -> snoc (v xs) x

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

-- | reverse on snoc list
rev :: [a] -> [a]
rev = ana psi
  where psi []     = Nothing
        psi (x:xs) = Just (xs, x)

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

cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs
assocl ::  (a, (b, c)) -> ((a, b), c)
assocl (a, (x, y)) = ((a, x), y)
assocr :: ((a, b), c) -> (a, (b, c))
assocr ((x, b), y) = (x, (b, y))
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


-- | select?
-- TODO: we can rewrite by using hylo because mapAccumR is cata and rev is ana
select :: Ord a => (a, [a]) -> (a, [a])
select (x, xs) = mapAccumR (x, f) (rev xs)
  where
    f (a, b) | a > b     = (b, a)
             | otherwise = (a, b)
