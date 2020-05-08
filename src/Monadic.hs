{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables, GADTs #-}
module Monadic where

import Control.Applicative (liftA2)
import Control.Monad ((<=<), (>=>), (=<<), (>>=))
import Control.Monad.Trans.Free
import Data.Functor.Foldable

paraM :: (Recursive t, Traversable (Base t), Monad m) => (Base t (t, a) -> m a) -> t -> m a
paraM alg = h
  where h = alg <=< traverse (liftA2 (liftA2 (,)) return h) . project

apoM :: (Monad m, Traversable (Base t), Corecursive t) => (a -> m (Base t (Either t a))) -> a -> m t
apoM coalg = h
  where h = (return . embed) <=< traverse (either return h) <=< coalg

anaM :: (Monad m, Traversable (Base t), Corecursive t) => (a -> m (Base t a)) -> a -> m t
anaM coalg = h
  where h = fmap embed . traverse h <=< coalg

-- futuM :: (Corecursive t, Traversable (Base t), Monad m) => (a -> m (Base t (Free (Base t) a))) -> a -> m t
futuM coalg = anaM go . Pure
  where
    go (Pure a)  = coalg a
    go (Free fa) = undefined

hyloM :: (Monad m, Traversable t) => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h
  where h = alg <=< traverse h <=< coalg

cataM :: (Monad m, Traversable (Base t), Recursive t) => (Base t a -> m a) -> t -> m a
cataM alg = h
  where h = alg <=< traverse h . project
