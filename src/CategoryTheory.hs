{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
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
newtype Identity a = Identity { runIdentity :: a }

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

class (Functor f, Functor u) => Adjunction f u where
  unit         :: a -> u (f a)
  counit       :: f (u a) -> a
  leftAdjunct  :: (f a -> b) -> a -> u b
  rightAdjunct :: (a -> u b) -> f a -> b

  unit           = leftAdjunct id'
  counit         = rightAdjunct id'
  leftAdjunct f  = fmap f * unit
  rightAdjunct f = counit * fmap f

-- Hom (AxB, C) == Hom (A, C^B)
instance Adjunction ((,) b) ((->) b) where
  leftAdjunct :: ((b, a) -> c) -> a -> b -> c
  leftAdjunct f a b = f (b, a)
  rightAdjunct :: (a -> b -> c) -> (b, a) -> c
  rightAdjunct f (b, a) = f a b

class (Functor' c d f, Functor' d c u) => Adjunction' c d f u where
  leftAdjunct'  :: d (f a) b -> c a (u b)
  rightAdjunct' :: c a (u b) -> d (f a) b

instance Monad m => Functor' (->) (Kleisli m) Identity where
  fmap' :: (a -> b) -> Kleisli m (Identity a) (Identity b)
  fmap' f = Kleisli $ fmap Identity . (pure . f . runIdentity)

instance Monad m => Functor' (Kleisli m) (->) m where
  fmap' :: Kleisli m a b -> m a -> m b
  fmap' k = (=<<) (runKleisli k)

instance Monad m => Adjunction' (->) (Kleisli m) Identity m where
  leftAdjunct' :: Kleisli m (Identity a) b -> a -> m b
  leftAdjunct' k a = runKleisli k (Identity a)

  rightAdjunct' :: (a -> m b) -> Kleisli m (Identity a) b
  rightAdjunct' f = Kleisli $ f . runIdentity

newtype Ran g h a = Ran { runRan :: forall b. (a -> g b) -> h b }

yonedaToRan :: Yoneda f a -> Ran Identity f a
yonedaToRan = undefined

ranToYoneda :: Ran Identity f a -> Yoneda f a
ranToYoneda = undefined

data Lan g h a where
  Lan :: (g b -> a) -> h b -> Lan g h a

{--
class Applicative m => Monad' m where
  join :: m (m a) -> m a
  join x = x `bind` id

  bind :: m a -> (a -> m b) -> m b
  m `bind` f = join (fmap f m)

  return :: a -> m a
  return = pure
--}

class Adjunction f u => Monad' f u where
  return' :: a -> u (f a)
  join' :: u (f (u (f a))) -> u (f a)

instance Monad' ((,) s) ((->) s) where
  return' :: a -> (s -> (s, a))
  return' a s = (s, a)

  join' :: (s -> (s, (s -> (s, a)))) -> (s -> (s, a))
  join' f s = f' s' where (s', f') = f s

class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id'

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f * duplicate

class Adjunction f u => Comonad' f u where
  extract' :: f (u a) -> a
  duplicate' :: f (u a) -> f (u (f (u a)))

instance Comonad' ((,) s) ((->) s) where
  extract' :: (s, s -> a) -> a
  extract' (s, f) = f s

  duplicate' :: (s, s -> a) -> (s, s -> (s, s -> a))
  duplicate' (s, f) = (s, \s -> (s, f))
