{-# LANGUAGE TemplateHaskell, TypeFamilies, KindSignatures #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase, BangPatterns #-}
module StudyICBICSort where

import Debug.Trace (trace)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

-- | isomorphic to Either
data Delayed a = Done a | Waiting (Delayed a) deriving Show
makeBaseFunctor ''Delayed
-- | isomorphic to either
delayed :: (a -> c) -> (b -> c) -> DelayedF a b -> c
delayed f g = u
  where u (DoneF n)    = f n
        u (WaitingF n) = g n

sort :: Ord a => [a] -> [a]
sort xs = hylo (delayed id id) psi ([], xs)

psi :: Ord a => ([a], [a]) -> DelayedF [a] ([a], [a])
psi (xs, ys) = case ys of
  []     -> DoneF xs
  (y:ys) -> WaitingF (zs++[w], ws)
    where (z, zs) = swap (y, xs)
          (w, ws) = swap (z, ys)

swap :: Ord a => (a, [a]) -> (a, [a])
swap (x, xs) = case break (x<) xs of
  (xs',   []) -> (x, xs')
  (xs', y:ys) -> (x, xs') <+> swap (y, ys)
    where (x, xs) <+> (y, ys) = (y, xs++[x]++ys)

sample = [1,3,2,5,4,7,6,0]
