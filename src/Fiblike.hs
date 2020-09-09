-- ref.) https://stackoverflow.com/questions/3208258/memoization-in-haskell
{-# LANGUAGE BangPatterns #-}
module Fiblike where

import Data.Function (fix)

naive_f :: Int -> Int
naive_f 0 = 0
naive_f n = max n (naive_f (n `div` 2) + naive_f (n `div` 3) + naive_f (n `div` 4))

-- f :: (Int -> Int) -> Int -> Int
f mf 0 = 0
f mf n = max n $ mf (n `div` 2) + mf (n `div` 3) + mf (n `div` 4)

f_list :: [Int]
f_list = map (f faster_f) [0..]

faster_f :: Int -> Int
faster_f n = f_list !! n

data Tree a = Tree (Tree a) a (Tree a) deriving Show
instance Functor Tree where
  fmap f (Tree l x r) = Tree (fmap f l) (f x) (fmap f r)

index :: Tree a -> Integer -> a
index (Tree _ x _) 0 = x
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
  (q, 0) -> index l q
  (q, 1) -> index r q

nats :: Tree Integer
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
      where l = n + s
            r = l + s
            s' = s * 2

toList :: Tree a -> [a]
toList xs = map (index xs) [0..]

f_tree :: Tree Integer
f_tree = fmap (f fastest_f) nats

fastest_f :: Integer -> Integer
fastest_f = index f_tree
