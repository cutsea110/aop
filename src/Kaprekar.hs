{-# LANGUAGE ScopedTypeVariables #-}
module Kaprekar where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Char (ord, chr)
import Data.List (sortBy, foldl')

toInt :: [Int] -> Int
toInt = foldl' (\a b -> 10 * a + b) 0

step :: String -> String
step cs = show cs'
  where
    nums = map (\x -> ord x - ord '0') cs
    (b, s) = (sortBy (flip compare) nums, sortBy compare nums)
    (b', s') = (toInt b, toInt s)
    cs' = b' - s'

kaprekar :: String -> [String]
kaprekar cs = cs : map snd seqs
  where
    sols = iterate step cs
    seqs = takeWhile (uncurry (/=)) $ zip sols (tail sols)

digits4 :: [String]
digits4 = map f ds
  where
    f = map (\x -> chr (x + ord '0'))
    ds = [[d1,d2,d3,d4] | d1 <- [0..9], d2 <- [0..9], d3 <- [0..9], d4 <- [0..9]]

check :: [(String, String)]
check = map ((head &&& last) . kaprekar) digits4

main = do
  cs <- getLine
  n :: Int  <- fmap read getLine
  let sols = iterate step cs
  print $ take n sols
