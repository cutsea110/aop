{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module StudyICBICSort where

import Debug.Trace (trace)

--------------------------------------------------------------------------------------
-- Fix point and morphisms
--------------------------------------------------------------------------------------
newtype Fix f = In { out :: f (Fix f) }
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = {- cata phi . ana psi -} phi . fmap (hylo phi psi) . psi
meta :: Functor f => (f a -> a) -> (a -> b) -> (b -> f b) -> Fix f -> Fix f
meta phi e psi = ana psi . e . cata phi -- In . fmap (meta phi e psi) . out
--------------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------------
debug = True

($?) :: Show a => (a -> b) -> a -> b
f $? x = if debug then trace (show x) (f x) else f x
--------------------------------------------------------------------------------------
-- Sort
--------------------------------------------------------------------------------------

type Sum a = Fix (SumF a)
data SumF a r = StopF a | PlayF r deriving (Show, Functor)

join :: (a -> c) -> (b -> c) -> SumF a b -> c
join f g = u
  where u (StopF n) = f n
        u (PlayF n) = g n

phi :: SumF a a -> a
phi = join id id

psi :: Ord a => ([a], [a]) -> SumF ([a], [a]) ([a], [a])
psi (xs, ys) = case ys of
  []     -> StopF (xs, [])
  (y:ys) -> PlayF (zs++[w], ws)
    where (z, zs) = swap (y, xs)
          (w, ws) = swap (z, ys)

swap :: Ord a => (a, [a]) -> (a, [a])
swap (x, xs) = case break (x<) xs of
  (xs',   []) -> (x, xs')
  (xs', y:ys) -> (x, xs') <+> swap (y, ys)
    where (x, xs) <+> (y, ys) = (y, xs++[x]++ys)

sort :: (Show a, Ord a) => [a] -> ([a], [a])
sort xs = hylo phi (psi $?) ([], xs)

instance Show a => Show (Sum a) where
  show (In x) = show x

sample :: [Integer]
sample = [1,3,2,5,4,7,6,0]
