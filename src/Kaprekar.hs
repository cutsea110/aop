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

digitsN :: Int -> [String]
digitsN = map f . sub
  where
    f = map (\x -> chr (x + ord '0'))
    sub :: Int -> [[Int]]
    sub = (map f [0..] !!)
      where
        f i | i == 0    = [[]]
            | otherwise = [x:xs | x <- [0..9], xs <- sub (i-1)]

check :: [(String, String)]
check = map ((head &&& last) . steps) $ digitsN 4

dump :: IO ()
dump = do
  let csv = intercalate "\n" $ map (mkCSV . ((id &&& step) &&& steps)) $ digitsN 4
  writeFile "kaprekar.csv" csv
    where
      mkCSV ((n, s), ss) =  n ++ "," ++ n ++ "," ++ s ++ "," ++ show (length ss)

manual :: IO ()
manual = do
  cs <- getLine
  print $ steps cs

main :: IO ()
main = do
  print "digits 2"
  print $ kaprekar 2
  print "digits 3"
  print $ kaprekar 3
  print "digits 4"
  print $ kaprekar 4
  print "digits 5"
  print $ kaprekar 5
  print "digits 6"
  print $ kaprekar 6
  where
    kaprekar = sub . digitsN
    sub = filter snd . map (id &&& isKaprekar)
