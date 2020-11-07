module Frog where

import Data.Array
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)

frog :: Int -> [Integer] -> Array Int Integer
frog n xs = arr where
  hs = listArray (1, n) xs
  idx = listArray (1, n) [1..n]
  arr = fmap (f faster_f) idx
    where faster_f = (arr !)
          f mf 1 = 0
          f mf 2 = abs $ hs!2 - hs!1
          f mf n = min (mf (n-1) + abs(hs!n - hs!(n-1))) (mf (n-2) + abs(hs!n - hs!(n-2)))

main = do
  n <- readLn :: IO Int
  xs <- fmap read . words <$> getLine
  print $ frog n xs ! n
