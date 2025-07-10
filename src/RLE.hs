module RLE where

import Data.List (unfoldr)

para :: b -> (a -> ([a], b) -> b) -> [a] -> b
para c f = u
  where u [] = c
        u (x:xs) = f x (xs, u xs)

toRLE :: Eq a => [a] -> [(a, Int)]
toRLE = unfoldr psi
  where
    psi :: Eq a => [a] -> Maybe ((a, Int), [a])
    psi = para Nothing f
      where
        f x (xs, Nothing) = Just ((x, 1), xs)
        f x (xs, Just ((y, n), zs))
          | x == y    = Just ((y, succ n), zs)
          | otherwise = Just ((x, 1), xs)
