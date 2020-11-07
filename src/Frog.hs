module Frog where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

parseInt = C.readInt . C.dropWhile isSpace
getIntVec n = V.unfoldrN n parseInt <$> C.getLine

main = do
  n <- readLn :: IO Int
  hs <- getIntVec n
  let f_vector = V.map (f faster_f) $ V.fromList [0..(n-1)]
        where
          f mf 0 = 0
          f mf 1 = abs $ hs V.! 1 - hs V.! 0
          f mf i = min (mf (i-1) + abs (hs V.! i - hs V.! (i-1))) (mf (i-2) + abs (hs V.! i - hs V.! (i-2)))
          faster_f = (f_vector V.!)

  print $ f_vector V.! (n-1)
