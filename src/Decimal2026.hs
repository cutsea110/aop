module Decimal2026 where

import Data.Char (chr, ord)
import Data.Ratio

y2026 :: Rational
y2026 = (2026 + 1 % 9999) / 9999

decimalDigits :: Rational -> [Int]
decimalDigits r = go (numerator r `mod` denominator r)
  where d = denominator r
        go rem = fromIntegral q : go rem'
          where (q, rem') = (rem * 10) `divMod` d

showDecimal :: Rational -> String
showDecimal r = show i ++ "." ++ map str ds
  where i   = numerator r `div` denominator r
        str = chr . (+ ord '0')
        ds  = decimalDigits r

main :: IO ()
main = putStrLn $ take (2+4*7972) $ showDecimal y2026 -- 7973 is enough to show the pattern
