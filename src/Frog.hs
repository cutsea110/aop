module Frog where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

parseInt = C.readInt . C.dropWhile isSpace
getIntVec n = V.unfoldrN n parseInt <$> C.getLine

n = 4
hs = V.fromList [10,40,30,20]

f mf 0 = 0
f mf 1 = abs $ hs V.! 1 - hs V.! 0
f mf n = min (mf (n-1) + abs (hs V.! n - hs V.! (n-1))) (mf (n-2) + abs (hs V.! n - hs V.! (n-2)))

f_vector = V.map (f faster_f) $ V.fromList [0..(n-1)]

faster_f = (f_vector V.!)

main = do
  n <- readLn :: IO Int
  hs <- getIntVec n
  print $ faster_f (n-1)
