module YearFloat where

import Data.Ratio

x :: Rational
x = (2026 + 1 % 9999) / 9999

decimalDigits :: Rational -> [Int]
decimalDigits r = go (numerator r `mod` denominator r)
  where
    d = denominator r
    go rem =
      let (q, rem') = (rem * 10) `divMod` d
      in fromIntegral q : go rem'

showDecimal :: Int -> Rational -> String
showDecimal n r =
  let i = numerator r `div` denominator r
      ds = decimalDigits r
  in show i ++ "." ++ concatMap show (take n ds)

main :: IO ()
main = putStrLn $ showDecimal 1000 x
