{-# LANGUAGE KindSignatures,
             TypeSynonymInstances,
             FlexibleInstances,
             RankNTypes,
             TypeOperators
#-}
module FixPrime where

import Prelude hiding (Functor(..), map, succ, either, subtract)
import qualified Control.Monad as M
import qualified Control.Comonad as C

dup f = (f, f)
tupply f = cross (dup f)
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
assocl (a, (b, c)) = ((a, b), c)
assocr ((a, b), c) = (a, (b, c))

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
para'' :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para'' f = snd . cata (pair (In . fmap fst, f))
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (either (id, apo psi)) . psi
apo' :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo' = cozygo out
-- histomorphism
histo :: Functor f => (f (Cofree f t) -> t) -> Fix f -> t
histo phi = extract . cata ap -- more efficient than histo'
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
-- zygomorphism is semi-mutually recursive function
zygo' :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo' f = mutu' (f . fmap fst)
-- cozygomorphism
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
cozygo f psi = ana (either (fmap Left . f, psi)) . Right
-- dynamorphism
dyna :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna f g = chrono f (fmap inject . g)

dyna' :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna' f g = histo f . ana g

-- ref.) http://titech-ssr.blog.jp/archives/1047835805.html
dyna'' :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna'' f g = extract . hylo ap g
  where
    ap a = Cf $ In $ Hisx (f a, fmap unCf a)

-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (Free f a)) -> a -> b
codyna f g = chrono (f . fmap extract) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi
-- Is this mutumorphism so pretty type signature? but there exists tuple is so concrete?
-- Does this mutu' use Fokkinga's mutual recursive theorem?
mutu' :: Functor f => (f (a, b) -> a) -> (f (a, b) -> b) -> Fix f -> b
mutu' f g = snd . cata (pair (f, g))
-- ref.) https://twitter.com/xgrommx/status/1259815344358797314 (@xgrommx's tweet)
-- but this do NOT have the type signature i expected
-- mutu'' :: Functor f => (f (a, a) -> a) -> (f (a, a) -> a) -> Fix f -> a
-- mutu'' f g = g . fmap (pair (mutu'' g f, mutu'' f g)) . out

-- This is the type signature i love. Here is a perfect one!
mutu'' :: Functor f => (f (a, b) -> b) -> (f (a, b) -> a) -> Fix f -> b
mutu'' = v
  where
    u :: Functor f => (f (a, b) -> a) -> (f (a, b) -> b) -> Fix f -> a
    u f g = f . fmap (pair (u f g, v g f)) . out

    v :: Functor f => (f (a, b) -> b) -> (f (a, b) -> a) -> Fix f -> b
    v g f = g . fmap (pair (u f g, v g f)) . out

-- comutumorphism
comutu :: Functor f => (b -> a) -> (a -> f a) -> b -> Fix f
comutu proj psi = ana psi . proj

-- ref.) https://twitter.com/xgrommx/status/1259815344358797314 (@xgrommx's tweet)
-- but this do NOT have the type signature i expected
-- comutu'' :: Functor f => (t -> f (Either t t)) -> (t -> f (Either t t)) -> t -> Fix f
-- comutu'' f g = In . (fmap (either (comutu'' g f, comutu'' f g))) . g

-- This is the type signature i love. Here is a perfect one!
comutu'' :: Functor f => (b -> f (Either a b)) -> (a -> f (Either a b)) -> b -> Fix f
comutu'' = v
  where
    u :: Functor f => (a -> f (Either a b)) -> (b -> f (Either a b)) -> a -> Fix f
    u f g = In . (fmap (either (u f g, v g f))) . f

    v :: Functor f => (b -> f (Either a b)) -> (a -> f (Either a b)) -> b -> Fix f
    v g f = In . (fmap (either (u f g, v g f))) . g



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


------------

-- ref.) https://blog.sumtypeofway.com/posts/recursion-schemes-part-6.html
-- ref.) https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms

-- | Mendler style catamorphism
--   Mendler's catamorphism don't need the Functor f constraint.
mcata :: ((Fix f -> a) -> f (Fix f) -> a) -> Fix f -> a
mcata phi = u
  where
    u = phi u . out

cata' :: Functor f => (f a -> a) -> Fix f -> a
cata' phi = mcata (\u -> phi . fmap u)

-- | generalized catamorphism
gcata :: (Functor f, C.Comonad w)
      => (forall b. f (w b) -> w (f b))
      -> (f (w a) -> a)
      -> Fix f -> a
gcata k g = C.extract . cata phi
  where phi = C.liftW g . k . fmap C.duplicate

-- | ekmett's recursion-schemes variant
gcata' :: (Functor f, C.Comonad w)
       => (forall b. f (w b) -> w (f b))
       -> (f (w a) -> a)
       -> Fix f -> a
gcata' k g = g . C.extract . c
  where c = k . fmap u . out
        u = C.duplicate . C.liftW g . c

-- | generalized anamorphism
gana :: (Functor f, Monad m)
     => (forall b. m (f b) -> f (m b))
     -> (a -> f (m a))
     -> a -> Fix f
gana k f = ana psi . return
  where psi = fmap M.join . k . M.liftM f
