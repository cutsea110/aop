{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module StudyICBICSort where

import Debug.Trace (trace)

--------------------------------------------------------------------------------------
newtype Fix f = In { out :: f (Fix f) }
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = {- cata phi . ana psi -} phi . fmap (hylo phi psi) . psi
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = ana psi . cata phi -- In . fmap (meta phi psi) . out
--------------------------------------------------------------------------------------
debug = True

($?) :: Show a => (a -> b) -> a -> b
f $? x = if debug then trace (show x) (f x) else f x
--------------------------------------------------------------------------------------

-- | recursion structure for Euclid's algorithm
-- data Euclidian a = Triv a | Same (Euclidian a) deriving Show
type Euclidian a = Fix (EuclidianF a)

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

sort :: (Show a, Ord a) => [a] -> ([a], [a])
sort xs = hylo phi (psi $?) ([], xs)

sort' :: (Show a, Ord a) => [a] -> Euclidian ([a], [a])
sort' xs = meta phi (psi $?) (In (TrivF ([], xs)))

instance Show a => Show (Fix (EuclidianF a)) where
  show (In x@(TrivF a)) = show x
  show (In x@(SameF a)) = show x

sample :: [Integer]
sample = [1,3,2,5,4,7,6,0]
