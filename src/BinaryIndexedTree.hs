{-# LANGUAGE DeriveFunctor, RankNTypes, GADTs #-}
module BinaryIndexedTree where

import Prelude hiding (sum)

import Control.Monad (forM_)
import Control.Monad.ST
import Control.Monad.Primitive (PrimState, PrimMonad)
import Data.Bits
import Data.Monoid
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

-- サンプルコード用
import Control.Arrow (second)
import Data.List ((\\))

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
But this use immutable Vector.
-}
inc :: Monoid a => Int -> a -> BIT a -> BIT a
inc i w b | 1 <= i && i <= n = b { vec = V.accum (<>) v $ zip xs (repeat w) }
          | otherwise = error $ "index must be between 1 to " ++ show n
  where (n, v) = (getN b, vec b)
        xs = takeWhile (<=n) $ iterate (\x -> x + x .&. (-x)) i

{- |
increment the value at index i(1-base) by amount w. O(log n)
this use mutable Vector.
-}
inc' :: Monoid a => Int -> a -> BIT a -> BIT a
inc' i w b =
  BIT n $ V.create $ do
    mv <- V.thaw v
    forM_ xs $ \i -> M.modify mv (<>w) i
    return mv
  where
    (n, v) = (getN b, vec b)
    xs = takeWhile (<=n) $ iterate (\x -> x + x .&. (-x)) i


{- |
lookup the sum of all values from index 1 to index i. O(log n)
-}
(!) :: Monoid a => BIT a -> Int -> a
b ! i | 1 <= i && i <= n = foldr (\i acc -> acc <> (v V.! i)) mempty xs
      | otherwise = error $ "index must be between 1 to " ++ show n
  where (n, v) = (getN b, vec b)
        xs = takeWhile (>0) $ iterate (\x -> x - x .&. (-x)) i

--------------------------------------------------------------------------------


{- | 転倒数
1-9 までの数だけでできたリストの転倒数をもとの数と組にして返す.

>>> inverse9 [4,1,5,2,6,3]
[(4,0),(1,1),(5,0),(2,2),(6,0),(3,3)]
-}
inverse9 :: [Int] -> [(Int, Int)]
inverse9 xs = map (second getSum) $ go xs b []
  where n = maximum xs
        b = new n :: BIT (Sum Int)
        go []     b acc = zip xs (reverse acc)
        go (x:xs) b acc = let acc' = b ! n - b ! x : acc
                              b' = inc' x 1 b
                          in go xs b' acc'

{- | 転倒対象リスト
1-9 までの数だけでできたリストの転倒している対象要素をリストアップ.

>>> inverse9' [4,1,5,2,6,3]
[(4,[]),(1,[4]),(5,[]),(2,[4,5]),(6,[]),(3,[4,5,6])]
-}
inverse9' :: [Int] -> [(Int, [Int])]
inverse9' xs = go xs b []
  where n = maximum xs
        b = new n :: BIT [Int]
        go []     b acc = zip xs (reverse acc)
        go (x:xs) b acc = let acc' = (b ! n \\ b ! x) : acc
                              b' = inc' x [x] b
                          in go xs b' acc'

{- | 転倒対象インデックスリスト
1-9 までの数だけでできたリストの転倒している対象要素をインデックス指定でリストアップ.

>>> inverse9'i [4,1,5,2,6,3]
[(4,[]),(1,[0]),(5,[]),(2,[0,2]),(6,[]),(3,[0,2,4])]
-}
inverse9'i :: [Int] -> [(Int, [Int])]
inverse9'i xs = go (zip [0..] xs) b []
  where n = maximum xs
        b = new n :: BIT [Int]
        go []             b acc = zip xs (reverse acc)
        go (ix@(i, x):xs) b acc = let acc' = (b ! n \\ b ! x) : acc
                                      b' = inc' x [i] b
                                  in go xs b' acc'
