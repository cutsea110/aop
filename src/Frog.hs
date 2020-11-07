module Frog where

import qualified Data.Vector as V

hs_list = [10,40,30,20]
hs = V.fromList hs_list
idx = [1..n]
  where n = length hs_list - 1

naive_f :: Int -> Int
naive_f 0 = 0
naive_f 1 = abs $ hs V.! 1 - hs V.! 0
naive_f n = min (naive_f (n-1) + abs (hs V.! n - hs V.! (n-1))) (naive_f (n-2) + abs (hs V.! n - hs V.! (n-2)))

f mf 0 = 0
f mf 1 = abs $ hs V.! 1 - hs V.! 0
f mf n = min (mf (n-1) + abs (hs V.! n - hs V.! (n-1))) (mf (n-2) + abs (hs V.! n - hs V.! (n-2)))

f_vector = V.map (f faster_f) $ V.fromList [0..3]

faster_f = (f_vector V.!)
