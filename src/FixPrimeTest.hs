module FixPrimeTest where

import Prelude hiding (Functor(..), map, succ, either)
import FixPrime

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
  

