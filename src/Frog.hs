module Frog where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as V

parseInt = C.readInt . C.dropWhile isSpace
getIntVec n = V.unfoldrN n parseInt <$> C.getLine

main = do
  n <- readLn :: IO Int
  hs <- getIntVec n
  let x#y = abs $ hs V.! x - hs V.! y
      f mf 0 = 0
      f mf 1 = abs $ hs V.! 1 - hs V.! 0
      f mf i = min (mf (i-1) + i#(i-1)) (mf (i-2) + i#(i-2))
      f_vector = V.map (f faster_f) $ V.fromList [0..(n-1)]
        where faster_f i = f_vector V.! i

  print $ f_vector V.! (n-1)
