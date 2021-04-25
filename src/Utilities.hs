module Utilities where

compose :: [a -> a] -> a -> a
compose = foldr (.) id

anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++ [x:xs' | xs' <- anyOne f xs]

segments as = [(as1, as2, as3)
              | (as1, bs) <- splits as
              , (as2, as3) <- splits bs
              ]

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (a:as) = [([], a:as)] ++ [(a:as1, as2) | (as1, as2) <- splits as]
