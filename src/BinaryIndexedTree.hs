{-# LANGUAGE DeriveFunctor #-}
module BinaryIndexedTree where

import Prelude hiding (sum)

import Data.Bits
import Data.Monoid
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

data BIT a = BIT { getN :: Int
                 , vec :: V.Vector a
                 }
           deriving Show

{- |
construct BIT by maximum value n.
-}
new :: Monoid a => Int -> BIT a
new n = BIT n (V.replicate (n+1) mempty) -- we can use from 1-index.

{- |
increment the value at index i(1-base) by amount w. O(log n)
-}
inc :: Monoid a => Int -> a -> BIT a -> BIT a
inc i w b | 1 <= i && i <= n = b { vec = V.accum (<>) v $ zip xs (repeat w) }
          | otherwise = error $ "index must be between 1 to " ++ show n
  where (n, v) = (getN b, vec b)
        xs = takeWhile (<=n) $ iterate (\x -> x + x .&. (-x)) i

{- |
lookup the sum of all values from index 1 to index i. O(log n)
-}
(!) :: Monoid a => BIT a -> Int -> a
b ! i | 1 <= i && i <= n = foldr (\i acc -> (v V.! i) <> acc) mempty xs
      | otherwise = error $ "index must be between 1 to " ++ show n
  where (n, v) = (getN b, vec b)
        xs = takeWhile (>0) $ iterate (\x -> x - x .&. (-x)) i
