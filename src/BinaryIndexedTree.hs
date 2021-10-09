{-# LANGUAGE DeriveFunctor #-}
module BinaryIndexedTree where

import Data.Bits
import qualified Data.Tree as T

data BIT a = BIT Int (Tree a) deriving (Show, Functor)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Functor)

toForest :: Tree a -> T.Forest a
toForest Empty = []
toForest (Node x l r) = [T.Node x (toForest l ++ toForest r)]

drawBIT :: Show a => BIT a -> IO ()
drawBIT (BIT k t) = do
  putStrLn (show k ++ " bits")
  putStr . T.drawForest . fmap (fmap show) . toForest $ t

gNew :: Monoid a => Int -> BIT a
gNew k = BIT k $ f k
  where f 0 = Empty
        f k = Node mempty fk' fk' where fk' = f (k-1)

gInc :: Monoid a => Int -> a -> BIT a -> BIT a
gInc i x (BIT k root) = BIT k $ f root (k-1) 0
  where f Empty _ _ = error "invalid arguments"
        f (Node y l r) j acc
          | i `testBit` j =
              if acc' == i
                  then  y'   `seq` Node y' l r
                  else  acc' `seq` Node y (f l j' acc') r
          | otherwise = y'   `seq` Node y' l (f r j' acc)
          where y' = x `mappend` y
                j' = j-1
                acc' = acc `setBit` j

gIndex :: Monoid a => BIT a -> Int -> a
gIndex (BIT k root) i = f root (k-1) mempty
  where f Empty _ acc = acc
        f (Node x l r) j acc
          | i `testBit` j = acc' `seq` f l j' acc'
          | otherwise     = f r j' acc
          where j'   = j-1
                acc' = acc `mappend` x

{- |
construct BIT on k bits. O(n)
-}
new :: Num a => Int -> BIT a
new k = BIT k $ f k
  where f 0 = Empty
        f k = Node 0 fk' fk' where fk' = f (k-1)

{- |
Lookup the sum of all values from index 1 to index i. O(log n)
-}
(!) :: Num a => BIT a -> Int -> a
BIT k root ! i = f root (k-1) 0
  where f Empty _ acc = acc
        f (Node x l r) j acc
          | i `testBit` j = acc' `seq` f l j' acc'
          | otherwise     = f r j' acc
          where j'   = j-1
                acc' = acc+x

{- |
Increment the value at index i(1-base) by amount x. O(log n)
-}
inc :: Num a => Int -> a -> BIT a -> BIT a
inc i x (BIT k root) = BIT k $ f root (k-1) 0
  where f Empty _ _ = error "invalid arguments"
        f (Node y l r) j acc
          | i `testBit` j =
              if acc' == i
                  then  y'   `seq` Node y' l r
                  else  acc' `seq` Node y (f l j' acc') r
          | otherwise = y'   `seq` Node y' l (f r j' acc)
          where y' = x+y
                j' = j-1
                acc' = acc `setBit` j

-----------------------------------------------
{- | 転倒数
1-9 までの数だけでできたリストの転倒数をもとの数と組にして返す.
-}
inverse9 :: [Int] -> [(Int, Int)]
inverse9 xs = go xs t []
  where
    t = new 4 -- 1-9 までなのでこれで十分
    go []     t acc = zip xs (reverse acc)
    go (x:xs) t acc = let acc' = t ! x : acc
                          t' = inc x 1 t
                      in go xs t' acc'
