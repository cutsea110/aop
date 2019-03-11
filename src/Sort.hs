module Sort where

import Data.List

-- selection sort
ssort :: Ord a => [a] -> [a]
ssort = unfoldr phi
  where
    phi [] = Nothing
    phi xs = let r@(y, ys) = (minimum xs, xs \\ [y]) in Just r -- select
