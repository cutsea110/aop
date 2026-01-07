module Decimal2026 where

import Data.Ratio

y2026 :: Rational
y2026 = (2026 + 1 % 9999) / 9999

decimalDigits :: Rational -> [Int]
decimalDigits r = go (numerator r `mod` denominator r)
  where d = denominator r
        go rem = fromIntegral q : go rem'
          where (q, rem') = (rem * 10) `divMod` d


showDecimal :: Int -> Rational -> String
showDecimal n r = show i ++ "." ++ concatMap show (take n ds)
  where i = numerator r `div` denominator r
        ds = decimalDigits r

main :: IO ()
main = putStrLn $ showDecimal 1000 y2026
