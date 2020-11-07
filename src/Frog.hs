module Frog where

import Data.Vector as V

hs = V.fromList [10,40,30,20]

naive_f :: Int -> Int
naive_f 0 = 0
naive_f 1 = abs $ hs V.! 1 - hs V.! 0
naive_f n = min (naive_f (n-1) + abs (hs V.! n - hs V.! (n-1))) (naive_f (n-2) + abs (hs V.! n - hs V.! (n-2)))
