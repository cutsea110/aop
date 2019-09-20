{-# LANGUAGE ExistentialQuantification,
             Rank2Types,
             AllowAmbiguousTypes
#-}
module Fix where

import Prelude hiding (either)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
either (f, g) (Left x) = f x
either (f, g) (Right x) = g x


newtype Fix f = In { out :: f (Fix f) }

newtype HisF f a = His { unHis :: (a, f (HisF f a)) }
extract :: HisF f t -> t
extract = fst . unHis

newtype FutF f a = Fut { unFut :: Either a (f (FutF f a)) }
inject :: a -> FutF f a
inject = Fut . Left

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = cata phi . ana psi -- phi . fmap (hylo phi psi) . psi
-- metamorphism
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = ana psi . cata phi -- In . fmap (meta phi psi) . out
-- paramorphism
para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para phi = phi . fmap (pair (id, para phi)) . out
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (either (id, apo psi)) . psi
-- histomorphism
histo :: Functor f => (f (HisF f t) -> t) -> Fix f -> t
histo phi = extract . cata (His . pair (phi, id))
-- futumorphism
futu :: Functor f => (t -> f (FutF f t)) -> t -> Fix f
futu psi = ana (either (psi, id) . unFut) . inject
-- chronomorphism
chrono :: Functor f => (f (HisF f b) -> b) -> (a -> f (FutF f a)) -> a -> b
chrono phi psi = histo phi . futu psi
-- zygomorphism
zygo :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo f phi = snd . cata (pair (f . fmap fst, phi))
-- cozygomorphism
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
cozygo f psi = ana (either (fmap Left . f, psi)) . Right
-- dynamorphism
dyna :: Functor f => (f (HisF f b) -> b) -> (a -> f a) -> a -> b
dyna f g = chrono f (fmap inject . g)
-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (FutF f a)) -> a -> b
codyna f g = chrono (f . fmap extract) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi
