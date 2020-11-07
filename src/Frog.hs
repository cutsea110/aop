module Frog where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

parseInt = C.readInt . C.dropWhile isSpace
getIntVec n = V.unfoldrN n parseInt <$> C.getLine

n = 6
hs_list = [30,10,60,10,60,50] -- [10,40,30,20]

f_vector = V.map (f faster_f) $ V.fromList [0..(n-1)]
  where
    hs = V.fromList hs_list
    f mf 0 = 0
    f mf 1 = abs $ hs V.! 1 - hs V.! 0
    f mf i = min (mf (i-1) + abs (hs V.! i - hs V.! (i-1))) (mf (i-2) + abs (hs V.! i - hs V.! (i-2)))
    faster_f = (f_vector V.!)

main = do
  n <- readLn :: IO Int
  hs <- getIntVec n
  print $ f_vector V.! (n-1)
