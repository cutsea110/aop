{-# LANGUAGE FlexibleContexts #-}
module Monadic where

import Control.Applicative (liftA2)
import Control.Monad ((<=<), (>=>), (=<<), (>>=))
import Control.Monad.Trans.Free
import Data.Functor.Foldable

type AlgebraM m f a = f a -> m a
type ParaAlgebraM m t a = Base t (t, a) -> m a
type CataM m t a = AlgebraM m (Base t) a -> t -> m a

paraM :: (Recursive t, Traversable (Base t), Monad m) => ParaAlgebraM m t a -> t -> m a
paraM alg = alg <=< traverse (liftA2 (liftA2 (,)) return (paraM alg)) . project

apoM :: (Monad m, Traversable (Base t), Corecursive t) => (a -> m (Base t (Either t a))) -> a -> m t
apoM coalg = (return . embed) <=< traverse (either return (apoM coalg)) <=< coalg

anaM :: (Monad m, Traversable (Base t), Corecursive t) => (a -> m (Base t a)) -> a -> m t
anaM f = fmap embed . traverse (anaM f) <=< f

{-
futuM :: (Corecursive t, Traversable (Base t), Monad m) => (a -> m (Base t (Free (Base t) a))) -> a -> m t
futuM coalg = anaM go . Pure
  where
    go (Pure a)  = coalg a
    go (Free fa) = return fa
-}

hyloM :: (Monad m, Traversable t) => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h
  where h = alg <=< traverse h <=< coalg

cataM :: (Monad f, Traversable (Base a), Recursive a) => CataM f a b
cataM f = (f =<<) . (traverse (cataM f)) . project
