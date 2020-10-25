module Approx where

approx n [] = []
approx n (x:xs) | n > 0 = x : approx (n-1) xs
