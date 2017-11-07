{-# LANGUAGE KindSignatures,
             TypeSynonymInstances,
             FlexibleInstances
#-}
module FixPrime where

import Prelude hiding (Functor(..), map, succ, either)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
either (f, g) (Left x) = f x
either (f, g) (Right x) = g x

class Bifunctor (f :: * -> * -> *) where
  bimap :: (a -> c, b -> d) -> f a b -> f c d

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

newtype Fix f = In { out :: f (Fix f) }

data Hisx f a x = Hisx (a, f x)
instance Functor f => Bifunctor (Hisx f) where
  bimap (g, h) (Hisx (a, x)) = Hisx (g a, fmap h x)
instance Functor f => Functor (Hisx f a) where
  fmap f = bimap (id, f)

-- | (Cofree f a) is Fixpoint for (Hisx f a)
newtype Cofree f a = Cf { unCf :: Fix (Hisx f a) }
instance Functor f => Functor (Cofree f) where
  fmap f = Cf . ana (phi . out) . unCf
    where
      phi (Hisx (a, x)) = Hisx (f a, x)

extract :: Functor f => Cofree f a -> a
extract cf = case out (unCf cf) of
  Hisx (a, _) -> a

sub :: Functor f => Cofree f a -> f (Cofree f a)
sub cf = case out (unCf cf) of
  Hisx (_, x) -> fmap Cf x

data Futx f a x = Futx { unFutx :: (Either a (f x)) }
instance Functor f => Bifunctor (Futx f) where
  bimap (g, h) (Futx (Left a)) = Futx (Left (g a))
  bimap (g, h) (Futx (Right x)) = Futx (Right (fmap h x))
instance Functor f => Functor (Futx f a) where
  fmap f = bimap (id, f)

-- | (Free f a) is Fixpoint for (Futx f a)
newtype Free f a = Fr { unFr :: Fix (Futx f a) }
instance Functor f => Functor (Free f) where
  fmap f = Fr . cata (In . phi) . unFr
    where
      phi (Futx (Left a)) = Futx (Left (f a))
      phi (Futx (Right x)) = Futx (Right x)

inject :: Functor f => a -> Free f a
inject = Fr . In . Futx . Left

-- | TODO: fixme
sup :: Functor f => f (Free f a) -> Free f a
sup fr = undefined

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = {- cata phi . ana psi -} phi . fmap (hylo phi psi) . psi
-- metamorphism
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = {- ana psi . cata phi -} In . fmap (meta phi psi) . out
-- paramorphism
para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para phi = phi . fmap (pair (id, para phi)) . out
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (either (id, apo psi)) . psi
-- histomorphism
histo :: Functor f => (f (Cofree f t) -> t) -> Fix f -> t
histo phi = extract . cata ap
  where
    ap = cast . Hisx . pair (phi, id)
    cast :: Functor f => Hisx f a (Cofree f a) -> Cofree f a
    cast = Cf . In . bimap (id, unCf)
histo' :: Functor f => (f (Cofree f t) -> t) -> Fix f -> t
histo' phi = phi . fmap u . out
  where
    -- Cf and Hisx cast newtype
    u = Cf . ana (Hisx . pair (histo' phi, out))
-- futumorphism
futu :: Functor f => (t -> f (Free f t)) -> t -> Fix f
futu psi = ana ap . inject
  where
    ap = either (psi, id) . unFutx . cast
    cast :: Functor f => Free f t -> Futx f t (Free f t)
    cast = bimap (id, Fr) . out . unFr

futu' :: Functor f => (t -> f (Free f t)) -> t -> Fix f
futu' psi = In . fmap u . psi
  where
    u = cata (either (futu' psi, In) . unFutx) . unFr
-- chronomorphism
chrono :: Functor f => (f (Cofree f b) -> b) -> (a -> f (Free f a)) -> a -> b
chrono phi psi = histo phi . futu psi
-- zygomorphism
zygo :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo f phi = snd . cata (pair (f . fmap fst, phi))
-- cozygomorphism
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
cozygo f psi = ana (either (fmap Left . f, psi)) . Right
-- dynamorphism
dyna :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna f g = chrono f (fmap inject . g)
-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (Free f a)) -> a -> b
codyna f g = chrono (f . fmap extract) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi
-- type functor
map :: (Bifunctor f, Functor (f a)) => (a -> c) -> Fix (f a) -> Fix (f c)
map f = cata (In . bimap (f, id))
