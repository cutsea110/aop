module Frog where

import Data.Array
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)

frog :: Int -> [Integer] -> Array Int Integer
frog n xs = arr where
  hs = listArray (1, n) xs
  i#j = abs(hs!i-hs!j)
  sub i j = arr!j + i#j
  arr = listArray (1, n) $ 0:abs(2#1):[min (arr!(i-2)+i#(i-2)) (arr!(i-1)+i#(i-1))| i <- [3..n]]

main = do
  n <- readLn :: IO Int
  xs <- fmap read . words <$> getLine
  print $ frog n xs ! n
