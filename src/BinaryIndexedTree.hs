{-# LANGUAGE DeriveFunctor #-}
module BinaryIndexedTree where

import Data.Bits
import Data.List (unfoldr)
import qualified Data.Tree as T

-- サンプルコードで必要なだけ
import Control.Arrow (second)
import Data.List ((\\))
import Data.Monoid (Sum(..))

-- UTILITY

bitWidth :: Int -> Int
bitWidth = length . toBit

toBit :: Int -> [Int]
toBit = reverse . unfoldr psi
  where
    psi n | n == 0 = Nothing
          | otherwise = let (q, r) = n `divMod` 2
                        in Just (r, q)

-- Bibary Indexed Tree

data BIT a = BIT Int (Tree a) deriving (Show, Functor)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Functor)

toForest :: Tree a -> T.Forest a
toForest Empty = []
toForest (Node x l r) = [T.Node x (toForest l ++ toForest r)]

drawBIT :: Show a => BIT a -> IO ()
drawBIT (BIT k t) = do
  putStrLn (show k ++ " bits")
  putStr . T.drawForest . fmap (fmap show) . toForest $ t

{- |
construct BIT on k bits. O(n)
-}
new :: Monoid a => Int -> BIT a
new k = BIT k $ f k
  where f 0 = Empty
        f k = Node mempty fk' fk' where fk' = f (k-1)

{- |
construct BIT by maximum value n.
-}
new' :: Monoid a => Int -> BIT a
new' n = new k
  where k = bitWidth n

{- |
Increment the value at index i(1-base) by amount x. O(log n)
-}
inc :: Monoid a => Int -> a -> BIT a -> BIT a
inc i x (BIT k root) = BIT k $ f root (k-1) 0
  where f Empty _ _ = error "invalid arguments"
        f (Node y l r) j acc
          | i `testBit` j =
              if acc' == i
                  then  y'   `seq` Node y' l r
                  else  acc' `seq` Node y (f l j' acc') r
          | otherwise = y'   `seq` Node y' l (f r j' acc)
          -- NOTE: If y `mappend` x for List Monoid, then we should reverse to return value at lookup by (!).
          where y' = x `mappend` y
                j' = j-1
                acc' = acc `setBit` j

{- |
Lookup the sum of all values from index 1 to index i. O(log n)
-}
(!) :: Monoid a => BIT a -> Int -> a
BIT k root ! i = f root (k-1) mempty
  where f Empty _ acc = acc
        f (Node x l r) j acc
          | i `testBit` j = acc' `seq` f l j' acc'
          | otherwise     = f r j' acc
          where j'   = j-1
                acc' = acc `mappend` x


-----------------------------------------------


{- | 転倒数
1-9 までの数だけでできたリストの転倒数をもとの数と組にして返す.
-}
inverse9 :: [Int] -> [(Int, Int)]
inverse9 xs = map (second getSum) $ go xs t []
  where
    t = new 4 -- 1-9 までなのでこれで十分
    go []     t acc = zip xs (reverse acc)
    go (x:xs) t acc = let acc' = t ! (2^4-1) - t ! x : acc
                          t' = inc x (Sum 1) t -- Monoid は Sum
                      in go xs t' acc'

{- | 転倒対象リスト
1-9 までの数だけでできたリストの転倒している対象要素をリストアップ
-}
inverse9' :: [Int] -> [(Int, [Int])]
inverse9' xs = go xs t []
  where
    t = new 4 -- 1-9 までなのでこれで十分
    go []          t acc = zip xs (reverse acc)
    go (x:xs) t acc = let acc' = (t ! (2^4-1) \\ t ! x) : acc
                          t' = inc x [x] t -- Monoid は [a]
                      in go xs t' acc'

{- | 転倒対象インデックスリスト
1-9 までの数だけでできたリストの転倒している対象要素をインデックス指定でリストアップ
-}
inverse9'i :: [Int] -> [(Int, [Int])]
inverse9'i xs = go (zip [0..] xs) t []
  where
    t = new 4 -- 1-9 までなのでこれで十分
    go []             t acc = zip xs (reverse acc)
    go (ix@(i, x):xs) t acc = let acc' = (t ! (2^4-1) \\ t ! x) : acc
                                  t' = inc x [i] t -- Monoid は [a]
                              in go xs t' acc'
