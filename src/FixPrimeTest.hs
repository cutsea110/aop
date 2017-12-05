{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances
#-}
module FixPrimeTest where

import Prelude hiding (Functor(..), map, succ, either, head, tail, init, last)
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
  fmap f = bimap (id, f)

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
  fmap f = bimap (id, f)

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
  fmap f = bimap (id, f)
  
-- | NonEmptyList a
data NonEmptyListF a x = Wrap a | Add a x deriving Show
type NonEmptyList a = Fix (NonEmptyListF a)

wrap :: a -> NonEmptyList a
wrap = In . Wrap
add :: a -> NonEmptyList a -> NonEmptyList a
add a x = In (Add a x)

instance Show a => Show (NonEmptyList a) where
  show (In (Wrap a)) = "(Wrap " ++ show a ++ ")"
  show (In (Add a x)) = "(Add " ++ show a ++ " " ++ show x ++ ")"

instance Bifunctor NonEmptyListF where
  bimap (f, g) (Wrap a) = Wrap (f a)
  bimap (f, g) (Add a x) = Add (f a) (g x)

instance Functor (NonEmptyListF a) where
  fmap f = bimap (id, f)

--
init :: List a -> List a
init (In Nil) = nil
init (In (Cons x (In Nil))) = nil
init (In (Cons x xs)) = cons x (init xs)

last :: List a -> a
last = para phi
  where
    phi (Cons x (In Nil, _)) = x
    phi (Cons _ (xs,     _)) = last xs

head :: List a -> a
head = para phi
  where
    phi (Cons x _) = x

tail :: List a -> List a
tail = para phi
  where
    phi Nil = nil
    phi (Cons _ (xs, _)) = xs

gen :: Int -> List Int
gen = ana phi
  where
    phi :: Int -> ListF Int Int
    phi n = if n == 0 then Nil else Cons n (n-1)
