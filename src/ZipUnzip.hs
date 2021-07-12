{- LANGUAGE NoImplicitPrelude -}
module ZipUnzip where

-- 逆により仕様
import Prelude hiding (zip, unzip, foldr)
-- import Data.List (unfoldr)

foldr :: (b, (a, b) -> b) -> [a] -> b
foldr phi@(c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr psi = v
  where v b = case psi b of
          Nothing      -> []
          Just (a, b') -> a:v b'

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr phi
  where phi = (nils, conss)
        nils = ([], [])
        conss ((a, b), (xs, ys)) = (a:xs, b:ys)

{-
unzip :: [(a, b)] -> ([a], [b])
unzip = foldr conss nils
  where nils = ([], [])
        conss (a, b) (xs, ys) = (a:xs, b:ys)
-}

zip :: ([a], [b]) -> [(a, b)]
zip = unfoldr psi
  where psi (a:xs, b:ys) = Just ((a, b), (xs, ys))
     -- psi ([], [])     = Nothing                 -- more strict
        psi _            = Nothing                 -- lax definition
        
unzip' [] = ([], [])
unzip' ((a,b):xys) = (a:xs, b:ys) where (xs, ys) = unzip' xys

zip' ([],[]) = []
zip' (a:xs, b:ys) = (a,b):xys where xys = zip' (xs,ys)

