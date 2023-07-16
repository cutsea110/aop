module Decimal where

{--
val' b m
  | m < b     = [m]
  | otherwise = val' b (m `div` b) ++ [m `mod` b]
--}

val' b m = f (m, [])
  where
    f (m, xs)
      | m < b     = m:xs
      | otherwise = f (m `div` b, m `mod` b:xs)
