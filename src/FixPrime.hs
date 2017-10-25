module FixPrime where

import Prelude hiding (Functor(..), map)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)


class Bifunctor (f :: * -> * -> *) where
  bimap :: (a -> c, b -> d) -> f a b -> f c d

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

newtype Fix f = In { out :: f (Fix f) }

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- type functor
map :: (Bifunctor f, Functor (f a)) => (a -> c) -> Fix (f a) -> Fix (f c)
map f = cata phi
  where
    phi = In . bimap (f, id)

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
  bimap = undefined

instance Functor (NatF ()) where
  fmap = undefined


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
  bimap = undefined

instance Functor (ListF a) where
  fmap = undefined

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
  bimap = undefined

instance Functor (TreeF a) where
  fmap = undefined
