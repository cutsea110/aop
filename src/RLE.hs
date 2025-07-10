module RLE where

import Data.List (unfoldr)

para :: b -> (a -> ([a], b) -> b) -> [a] -> b
para c f = u
  where u [] = c
        u (x:xs) = f x (xs, u xs)

toRLE :: Eq a => [a] -> [(a, Int)]
toRLE = unfoldr coalg
  where
    coalg :: Eq a => [a] -> Maybe ((a, Int), [a])
    coalg = para Nothing alg
      where
        alg x (xs, Nothing) = Just ((x, 1), xs)
        alg x (xs, Just ((y, n), zs))
          | x == y    = Just ((y, succ n), zs)
          | otherwise = Just ((x, 1), xs)
