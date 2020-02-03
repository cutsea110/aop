{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module CategoryTheory where

import Prelude hiding (id, (.), (*))

id' :: a -> a
id' a = a
(*) :: (b -> c) -> (a -> b) -> (a -> c)
f * g = \x -> f (g x)

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
  id = Lens id' (const id')

  (.) :: Lens b c -> Lens a b -> Lens a c
  Lens g1 s1 . Lens g2 s2 = Lens (g1 * g2) s3
    where
      s3 a c = s2 a (s1 (g2 a) c)

class (Category c, Category d) => Functor' c d f where
  fmap' :: c a b -> d (f a) (f b)

instance Functor' (->) (->) Maybe where
  fmap' _ Nothing  = Nothing
  fmap' f (Just a) = Just (f a)

newtype f :~> g = NT { unNT :: forall x. f x -> g x }
{--
instance Category (:~>) where
  id :: a :~> a
  id = NT id'
  (.) :: (b :~> c) -> (a :~> b) -> (a :~> c)
  NT f . NT g = NT (f * g)
--}

data (f :+: g) e = InL (f e) | InR (g e)

data Const a b = Const a
data Identity a = Identity a

toMaybe :: (Const () :+: Identity) :~> Maybe
toMaybe = undefined
fromMaybe :: Maybe :~> (Const () :+: Identity)
fromMaybe = undefined

newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))

liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda = undefined
lowerYoneda :: Yoneda f a -> f a
lowerYoneda = undefined

data CoYoneda f a where
  CoYoneda :: (z -> a) -> f z -> CoYoneda f a

instance Functor (CoYoneda f) where
  fmap f (CoYoneda g v) = CoYoneda (f * g) v

liftCoYoneda :: f a -> CoYoneda f a
liftCoYoneda = undefined
lowerCoYoneda :: Functor f => CoYoneda f a -> f a
lowerCoYoneda = undefined

-- covariant Yoneda
-- f a == forall b. (a -> b) -> f b

-- contravariant Yoneda
-- f a == exists b. (b -> a, f b)

-- covariant Coyoneda
-- f a == forall b. (b -> a) -> f b

-- contravariant Coyoneda
-- f a == exists b. (a -> b, f b)
