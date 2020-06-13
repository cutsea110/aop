-- O(N)?
-- ref.) https://scrapbox.io/Haskell-Misc/%E4%B8%AD%E5%A4%AE%E5%80%A4_O(N)%3F
-- by nobsun
module Median where

import Data.Bool

median :: Ord a => [a] -> [a]
median = snd3 . foldl med ([], [], [])
  where
    med ([], [], []) z = ([], [z], [])
    med (xs, [y], ys) z
      | y <= z = case insert True z ys of
          y':ys' -> (xs, [y,y'], ys')
      | otherwise = case insert False z xs of
          x':xs' -> (xs', [x',y], ys)
    med (xs,[x,y],ys) z
      | z < x = (insert False z xs, [x], y:ys)
      | y < z = (x:xs, [y], insert True z ys)
      | otherwise = (x:xs, [z], y:ys)

insert :: Ord a => Bool -> a -> [a] -> [a]
insert _ x [] = [x]
insert inc x yys@(y:ys)
  = bool (y:insert inc x ys) (x:yys) (bool (x>=y) (x<=y) inc)

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
