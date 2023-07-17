module Decimal where

import Data.Char (chr, ord)

val :: Int -> [Int] -> Int
val b = foldl phi 0
  where
    phi n d = n * b + d

val' :: Int -> Int -> [Int]
val' b m = f (m, [])
  where
    f (m, xs)
      | m < b     = m:xs
      | otherwise = f (m `div` b, m `mod` b:xs)

bin, oct, dec, hex :: [Int] -> Int
bin = val 2
oct = val 8
dec = val 10
hex = val 16

bin', oct', dec', hex' :: Int -> [Int]
bin' = val' 2
oct' = val' 8
dec' = val' 10
hex' = val' 16

toChar :: Int -> Char
toChar n
  | 0 <= n && n <= 9 = chr (ord '0' + n)
  | 10 <= n && n <= 15 = chr (ord 'a' + n - 10)
