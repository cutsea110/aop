{-# LANGUAGE KindSignatures,
             TypeSynonymInstances,
             FlexibleInstances,
             RankNTypes,
             TypeOperators
#-}
module FixPrime where

import Prelude hiding (Functor(..), map, succ, either, subtract)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
either (f, g) (Left x) = f x
either (f, g) (Right x) = g x
left :: Either l r -> l
left (Left x) = x
right :: Either l r -> r
right (Right x) = x
first f (x, y) = (f x, y)
second f (x, y) = (x, f y)
outl (x, y) = x
outr (x, y) = y

class Bifunctor (f :: * -> * -> *) where
  bimap :: (a -> c, b -> d) -> f a b -> f c d

class Bifunctor (f :: * -> * -> *) => ApplicativeBifunctor f where
  biap :: f (a -> c) (b -> d) -> f a b  -> f c d

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
  Hisx (_, b) -> fmap Cf b

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

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = phi . fmap (hylo phi psi) . psi
hylo' :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo' phi psi = cata phi . ana psi
-- metamorphism
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = In . fmap (meta phi psi) . out
meta' :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta' phi psi = ana psi . cata phi
-- paramorphism
para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para phi = phi . fmap (pair (id, para phi)) . out
para' :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para' = zygo In
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (either (id, apo psi)) . psi
apo' :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo' = cozygo out
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

chrono' :: Functor f => (f (Cofree f b) -> b) -> (a -> f (Free f a)) -> a -> b
chrono' phi psi = extract . hylo phi' psi' . inject
  where
    phi' = toCofree . Hisx . pair (phi, id)
    toCofree = Cf . In . bimap (id, unCf)
    psi' = either (psi, id) . unFutx . fromFree
    fromFree = bimap (id, Fr) . out . unFr
-- cochronomorphism
cochrono :: Functor f => (f (Cofree f t) -> t) -> (t -> f (Free f t)) -> Fix f -> Fix f
cochrono phi psi = futu psi . histo phi
-- synchromorphism
{--
synchro :: (Functor m, Functor n, Bifunctor f, Bifunctor g, Bifunctor h, Bifunctor j, Bifunctor k) =>
  (m c -> c, c -> n c) ->
  (h x (Either a c) -> m c) ->
  (f x a -> a, g x a -> h x a) ->
  ((h x a, b) -> k x b) -> 
  ((h x a, j x b) -> h x (Either a (g x a, b))) -> 
  (k x b -> b, b -> j x b) -> 
  (g x a, b) -> c
--}
{-
synchro ::
  Bialg m n c ->
  (h x (Either a c) -> m c) ->
  Trialg (f x) (g x) (h x) a ->
  ((h x a, b) -> k x b) ->
  ((h x a, j x b) -> h x (Either a (g x a, b))) ->
  Bialg (k x) (j x) b ->
  (g x a, b) ->
  c
-}
synchro :: (b3 -> t5, b4)
     -> ((t4, (t3, t5)) -> b3)
     -> (a, t2 -> t1)
     -> ((t1, b1) -> b2)
     -> ((t1, t) -> (t4, (t3, (t2, b1))))
     -> (b2 -> b, b -> t)
     -> (t2, b1)
     -> t5
synchro d' f d g1 g2 d'' = h
  where
    h = fst d' . f . second (second h) . g2 . pair (fst, (snd d'' . fst d'' . g1)) . first (snd d)

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
-- comutumorphism
comutu :: Functor f => (b -> a) -> (a -> f a) -> b -> Fix f
comutu proj psi = ana psi . proj
-- prepromorphism
type f :~> g = forall a. f a -> g a
-- supermap
cascade :: (Bifunctor f, Functor (f a)) => (a -> a) -> Fix (f a) -> Fix (f a)
cascade f = In . cata (bimap (id, map f . In))

cascade' :: (Bifunctor f, Functor (f a)) => (a -> a) -> Fix (f a) -> Fix (f a)
cascade' f = In . bimap (id, cascade' f . map f) . out
-- prepro :: Functor f => (f (Fix f) -> f (Fix f)) -> (f a -> a) -> Fix f -> a
prepro :: Functor f => (f :~> f) -> (f a -> a) -> Fix f -> a
prepro h alg = alg . fmap (prepro h alg . cata (In . h)) . out
-- postpromorphism
-- iterate?
iterate :: (Bifunctor f, Functor (f a)) => (a -> a) -> Fix (f a) -> Fix (f a) 
iterate f = ana (bimap (id, out . map f)) . out

-- postpro :: Functor f => (f (Fix f) -> f (Fix f)) -> (a -> f a) -> a -> Fix f
postpro :: Functor f => (f :~> f) -> (a -> f a) -> a -> Fix f
postpro h coalg = In . fmap (ana (h . out) . postpro h coalg) . coalg
-- exomorphism
--- type Alg f a = f a -> a
--- type Coalg f a = a -> f a
--- type Bialg f g a = (Alg f a, Coalg g a)
--- type Dialg f g a = f a -> g a
--- type Trialg f g h a = (Alg f a, Dialg g h a)
--- exo :: Functor h => Bialg m n b -> Dialg h m b -> (h a -> h (g a)) -> Trialg f g h a -> g a -> b
exo :: Functor h =>
  (m b -> b, b -> n b) -> (h b -> m b) -> (h a -> h (g a)) -> (f a -> a, g a -> h a) -> g a -> b
exo c f g d = cata (fst c . f) . ana (g . snd d)
exo' :: Functor h =>
  (m b -> b, b -> n b) -> (h b -> m b) -> (h a -> h (g a)) -> (f a -> a, g a -> h a) -> g a -> b
exo' c f g d = hylo (fst c . f) (g . snd d)
-- type functor
map :: (Bifunctor f, Functor (f a)) => (a -> c) -> Fix (f a) -> Fix (f c)
map f = cata (In . bimap (f, id))

map' :: (Functor (f c), Bifunctor f) => (a -> c) -> Fix (f a) -> Fix (f c)
map' f = ana (bimap (f, id) . out)
