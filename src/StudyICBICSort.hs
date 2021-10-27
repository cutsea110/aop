{-# LANGUAGE DeriveFunctor #-}
module StudyICBICSort where

import Debug.Trace (trace)

import Fix

debug = True

($?) :: Show a => (a -> b) -> a -> b
f $? x = if debug then trace (show x) (f x) else f x


-- | recursion structure for Euclid's algorithm
data Euclidian a = Triv a | Same (Euclidian a) deriving Show

data EuclidianF a r = TrivF a | SameF r deriving (Show, Functor)

-- | isomorphic to either, EuclidianF is a base functor of Euclidian
euclidian :: (a -> c) -> (b -> c) -> EuclidianF a b -> c
euclidian f g = u
  where u (TrivF n) = f n
        u (SameF n) = g n

phi :: EuclidianF a a -> a
phi = euclidian id id

psi :: Ord a => ([a], [a]) -> EuclidianF ([a], [a]) ([a], [a])
psi (xs, ys) = case ys of
  []     -> TrivF (xs, [])
  (y:ys) -> SameF (zs++[w], ws)
    where (z, zs) = swap (y, xs)
          (w, ws) = swap (z, ys)

swap :: Ord a => (a, [a]) -> (a, [a])
swap (x, xs) = case break (x<) xs of
  (xs',   []) -> (x, xs')
  (xs', y:ys) -> (x, xs') <+> swap (y, ys)
    where (x, xs) <+> (y, ys) = (y, xs++[x]++ys)

sample = [1,3,2,5,4,7,6,0]

sort :: (Show a, Ord a) => [a] -> [a]
sort xs = fst . hylo phi (psi $?) $ ([], xs)
