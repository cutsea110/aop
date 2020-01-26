{-# LANGUAGE InstanceSigs #-}
module CategoryTheory where

import Prelude hiding (id, (.))

class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id :: (->) a a
  id a = a

  (.) :: (->) b c -> (->) a b -> (->) a c
  f . g = \x -> f (g x)

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
  id :: (Kleisli m) a a
  id = Kleisli pure

  (.) :: (Kleisli m) b c -> (Kleisli m) a b -> (Kleisli m) a c
  f . g = Kleisli (\x -> runKleisli g x >>= runKleisli f)
