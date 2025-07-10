{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Kaprekar where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Char (ord, chr)
import Data.Function (on)
import Data.List (sortBy, foldl')

toInt :: [Int] -> Int
toInt = foldl' (\a b -> 10*a+b) 0

step :: String -> String
step cs = replicate (diff cs cs') '0' ++ cs'
  where
    diff = (-) `on` length
    nums = map (\x -> ord x - ord '0') cs
    (b, s) = (toInt . sortBy (flip compare) &&& toInt . sortBy compare) nums
    cs' = show $ b - s

steps :: String -> [String]
steps cs = cs : map snd seqs
  where
    sols = iterate step cs
    seqs = takeWhile (uncurry (/=)) $ zip sols (tail sols)

isKaprekar :: String -> Bool
isKaprekar = (==) <$> id <*> step

digitsN :: Int -> [String]
digitsN = map f . sub
  where
    f = map (\x -> chr (x + ord '0'))
    sub :: Int -> [[Int]]
    sub = (map f [0..] !!)
      where
        f i | i == 0    = [[]]
            | otherwise = [x:xs | x <- [0..9], xs <- sub (i-1)]

check :: Int -> [(String, String)]
check = map ((head &&& last) . steps) . digitsN

-- MAIN

main :: IO ()
main = forM_ [2..6] $ \i -> do
  putStrLn $ "digits " ++ show i
  print $ kaprekar i
  where
    kaprekar = sub . digitsN
    sub = filter snd . map (id &&& isKaprekar)
