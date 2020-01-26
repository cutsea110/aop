{-# LANGUAGE InstanceSigs #-}
module CategoryTheory where

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

data Lens a b = Lens (a -> b) (a -> b -> a)

instance Category Lens where
  id :: Lens a a
  id = Lens Prelude.id (const Prelude.id)

  (.) :: Lens b c -> Lens a b -> Lens a c
  Lens g1 s1 . Lens g2 s2 = Lens (g1 Prelude.. g2) s3
    where
      s3 a c = s2 a (s1 (g2 a) c)
