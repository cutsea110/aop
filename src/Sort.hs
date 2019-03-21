module Sort where

import Data.List

-- selection sort
ssort :: Ord a => [a] -> [a]
ssort = unfoldr phi
  where
    phi [] = Nothing
    phi xs = let r@(y, ys) = (minimum xs, xs \\ [y]) in Just r -- select

data Tree a = Null | Fork (Tree a, a, Tree a) deriving (Show, Eq)

foldt (c, f) = u
  where
    u Null = c
    u (Fork (lt, n, rt)) = f (u lt, n, u rt)

flatten = foldt (nil, join)
nil = []
join (x, a, y) = x ++ [a] ++ y
