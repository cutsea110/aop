-- | ref.) http://titech-ssr.blog.jp/archives/1047835805.html
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module KnapSack where

import Prelude hiding (Functor)
import FixPrime

-- KSTree a = (Int, Int) x (Maybe a)
--          = (Int, Int) x (1 + a)
--          = (Int, Int) + (Int, Int) x a
--
-- Fix KSTree == List (Int, Int)
data KSTreeF a = KSTreeF (Int, Int) (Maybe a) deriving Show
type KSTree = Fix KSTreeF

instance Functor KSTreeF where
  fmap f (KSTreeF a Nothing)  = KSTreeF a Nothing
  fmap f (KSTreeF a (Just b)) = KSTreeF a (Just (f b))

instance Show KSTree where
--  show (In (KSTreeF a b))  = "KSTree " ++ show a ++ "(" ++ show b ++ ")"
  show (In (KSTreeF a Nothing))   = show a
  show (In (KSTreeF a (Just b)))  = show a ++ "," ++ show b


knapsack :: Int -> [Int] -> [Int] -> Int
knapsack c v w = dyna phi psi (n, c)
  where
    n = length w

    psi (0, 0) = KSTreeF (n,   0) Nothing
    psi (0, j) = KSTreeF (n,   j) (Just (n, j-1))
    psi (i, j) = KSTreeF (n-i, j) (Just (i-1, j))

    phi (KSTreeF _ Nothing) = 0
    phi (KSTreeF (i, j) (Just cs))
      | i == n = 0
      | w !! i <= j = max x1 x2
      | otherwise = x1
      where
        x1 = back 1 cs
        x2 = (v !! i) + (back (1 + (n+1) * (w !! i)) cs)

    back i cs
      | i == 1 = extract cs
      | otherwise = case sub cs of
          (KSTreeF _ (Just b)) -> back (i-1) b

main = do
  print $ knapsack 5 [4,2,5,8] [2,2,1,3]
  print $ knapsack 8 [30,50,60] [3,4,5]
  print $ knapsack 15 [5,6,4,6,5,2] [6,5,6,6,3,7]
