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
