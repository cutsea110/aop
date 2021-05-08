{-# LANGUAGE ScopedTypeVariables #-}
module Kaprekar where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Char (ord, chr)
import Data.List (sortBy, foldl', intercalate)

toInt :: [Int] -> Int
toInt = foldl' (\a b -> 10 * a + b) 0

step :: String -> String
step cs = replicate (l-l') '0' ++ cs'
  where
    (l, l') = (length cs, length cs')
    nums = map (\x -> ord x - ord '0') cs
    (b, s) = (toInt . sortBy (flip compare) &&& toInt . sortBy compare) nums
    cs' = show $ b - s

isKaprekar :: String -> Bool
isKaprekar = (==) <$> id <*> step

steps :: String -> [String]
steps cs = cs : map snd seqs
  where
    sols = iterate step cs
    seqs = takeWhile (uncurry (/=)) $ zip sols (tail sols)

digits2 :: [String]
digits2 = map f ds
  where
    f = map (\x -> chr (x + ord '0'))
    ds = [[d1,d2] | d1 <- [0..9], d2 <- [0..9]]

digits3 :: [String]
digits3 = map f ds
  where
    f = map (\x -> chr (x + ord '0'))
    ds = [[d1,d2,d3] | d1 <- [0..9], d2 <- [0..9], d3 <- [0..9]]

digits4 :: [String]
digits4 = map f ds
  where
    f = map (\x -> chr (x + ord '0'))
    ds = [[d1,d2,d3,d4] | d1 <- [0..9], d2 <- [0..9], d3 <- [0..9], d4 <- [0..9]]

digits5 :: [String]
digits5 = map f ds
  where
    f = map (\x -> chr (x + ord '0'))
    ds = [[d1,d2,d3,d4,d5] | d1 <- [0..9], d2 <- [0..9], d3 <- [0..9], d4 <- [0..9], d5 <- [0..9]]

digits6 :: [String]
digits6 = map f ds
  where
    f = map (\x -> chr (x + ord '0'))
    ds = [[d1,d2,d3,d4,d5,d6] | d1 <- [0..9], d2 <- [0..9], d3 <- [0..9], d4 <- [0..9], d5 <- [0..9], d6 <- [0..9]]

check :: [(String, String)]
check = map ((head &&& last) . steps) digits4

dump :: IO ()
dump = do
  let csv = intercalate "\n" $ map (mkCSV . ((id &&& step) &&& steps)) digits4
  writeFile "kaprekar.csv" csv
    where
      mkCSV ((n, s), ss) =  n ++ "," ++ n ++ "," ++ s ++ "," ++ show (length ss)

main :: IO ()
main = do
  cs <- getLine
  print $ steps cs
