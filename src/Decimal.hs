module Decimal where

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

bin  = val  2
bin' = val' 2
oct  = val  8
oct' = val' 8
dec  = val  10
dec' = val' 10
hex  = val  16
hex' = val' 16
