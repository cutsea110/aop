module Hamming where

merge :: Ord a => [a] -> [a] -> [a]
merge xxs@(x:xs) yys@(y:ys) | x < y = x : merge xs yys
                            | x == y = x : merge xs ys
                            | otherwise = y : merge xxs ys

-- hamming = 1 : merge (merge [2*x|x<-fs] [3*x|x<-fs]) [5*x|x<-fs]
--   where fs = hamming

hamming = 1 : foldl1 merge (map ((`map` hamming) . (*)) [2,3,5])
