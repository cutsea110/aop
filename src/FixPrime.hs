{-# LANGUAGE KindSignatures,
             TypeSynonymInstances,
             FlexibleInstances
#-}
module FixPrime where

import Prelude hiding (Functor(..), map, succ, either, head, last)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
either (f, g) (Left x) = f x
either (f, g) (Right x) = g x

class Bifunctor (f :: * -> * -> *) where
  bimap :: (a -> c, b -> d) -> f a b -> f c d

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

newtype Fix f = In { out :: f (Fix f) }

data Hisx f a x = Hisx a (f x)
instance Functor f => Bifunctor (Hisx f) where
  bimap (g, h) (Hisx a x) = Hisx (g a) (fmap h x)
instance Functor f => Functor (Hisx f a) where
  fmap f = bimap (id, f)

-- | (Cofree f a) is Fixpoint for (Hisx f a)
newtype Cofree f a = Cf { unCf :: Fix (Hisx f a) }
instance Functor f => Functor (Cofree f) where
  fmap f = Cf . ana (phi . out) . unCf
    where
      phi (Hisx a x) = Hisx (f a) x

extract :: Functor f => Cofree f a -> a
extract cf = case out (unCf cf) of
  Hisx a _ -> a

sub :: Functor f => Cofree f a -> f (Cofree f a)
sub cf = case out (unCf cf) of
  Hisx _ x -> fmap Cf x

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

type FutF' f a = Fix (Futx f a)

newtype FutF f a = Fut { unFut :: Either a (f (FutF f a)) }
last :: a -> FutF f a
last = Fut . Left

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
    ap a = Cf (In (Hisx (phi a) (fmap unCf a)))
histo' :: Functor f => (f (Cofree f t) -> t) -> Fix f -> t
histo' phi = phi . fmap u . out
  where
    u = Cf . ana (uncurry Hisx . pair (histo' phi, out))
-- futumorphism
futu :: Functor f => (t -> f (FutF f t)) -> t -> Fix f
futu psi = ana (either (psi, id) . unFut) . last
futu' :: Functor f => (t -> f (Free f t)) -> t -> Fix f
futu' psi = undefined
-- chronomorphism
chrono :: Functor f => (f (Cofree f b) -> b) -> (a -> f (FutF f a)) -> a -> b
chrono phi psi = histo phi . futu psi
-- zygomorphism
zygo :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo f phi = snd . cata (pair (f . fmap fst, phi))
-- cozygomorphism
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
cozygo f psi = ana (either (fmap Left . f, psi)) . Right
-- dynamorphism
dyna :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna f g = chrono f (fmap last . g)
-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (FutF f a)) -> a -> b
codyna f g = chrono (f . fmap extract) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi
-- type functor
map :: (Bifunctor f, Functor (f a)) => (a -> c) -> Fix (f a) -> Fix (f c)
map f = cata (In . bimap (f, id))

-- | Natural Number
data NatF a x = Zero | Succ a x deriving (Show)
type Nat = Fix (NatF ())

zero :: Nat
zero = In Zero
succ :: Nat -> Nat
succ n = In (Succ () n)

instance Show Nat where
  show (In Zero) = "Zero"
  show (In (Succ () (In Zero))) = "Succ Zero"
  show (In (Succ () n)) = "Succ (" ++ show n ++ ")"

instance Bifunctor NatF where
  bimap (f, g) Zero = Zero
  bimap (f, g) (Succ x y) = Succ (f x) (g y)

instance Functor (NatF ()) where
  fmap f Zero = Zero
  fmap f (Succ () y) = Succ () (f y)


-- | List a
data ListF a x = Nil | Cons a x deriving (Show)
type List a = Fix (ListF a)

nil :: List a
nil = In Nil
cons :: a -> List a -> List a
cons x xs = In (Cons x xs)

instance Show a => Show (List a) where
  show x = "(" ++ show (out x) ++ ")"

instance Bifunctor ListF where
  bimap (f, g) Nil = Nil
  bimap (f, g) (Cons a x) = Cons (f a) (g x)

instance Functor (ListF a) where
  fmap f Nil = Nil
  fmap f (Cons a x) = Cons a (f x)

-- | Tree a
data TreeF a x = Tip a | Bin x x deriving (Show)
type Tree a = Fix (TreeF a)

tip :: a -> Tree a
tip = In . Tip
bin :: Tree a -> Tree a -> Tree a
bin l r = In (Bin l r)

instance Show a => Show (Tree a) where
  show (In (Tip x)) = "Tip " ++ show x
  show (In (Bin l r)) = "Bin (" ++ show l ++ ") (" ++ show r ++ ")"

instance Bifunctor TreeF where
  bimap (f, g) (Tip x) = Tip (f x)
  bimap (f, g) (Bin l r) = Bin (g l) (g r)

instance Functor (TreeF a) where
  fmap f (Tip x) = Tip x
  fmap f (Bin l r) = Bin (f l) (f r)
  
