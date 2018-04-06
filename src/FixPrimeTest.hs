{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances
#-}
module FixPrimeTest where

import Prelude hiding (Functor(..),
                       map,
                       succ,
                       either,
                       head,
                       tail,
                       init,
                       last,
                       takeWhile,
                       dropWhile,
                       zip,
                       unzip
                      )
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

instance ApplicativeBifunctor NatF where
  biap Zero Zero = Zero
  biap (Succ f g) (Succ x y) = Succ (f x) (g y)

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

instance ApplicativeBifunctor ListF where
  biap Nil Nil = Nil
  biap (Cons f g) (Cons x y) = Cons (f x) (g y)

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

instance ApplicativeBifunctor TreeF where
  biap (Tip f) (Tip x) = Tip (f x)
  biap (Bin f g) (Bin l r) = Bin (f l) (g r)

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

instance ApplicativeBifunctor NonEmptyListF where
  biap (Wrap f) (Wrap x) = Wrap (f x)
  biap (Add f g) (Add x y) = Add (f x) (g y)

--
init :: List a -> List a
init = para phi
  where
    phi (Cons x (In Nil, _)) = nil
    phi (Cons x (xs,    ys)) = cons x ys

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

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p = cata phi
  where
    phi Nil = nil
    phi (Cons x xs) | p x       = cons x xs
                    | otherwise = nil

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p = para phi
  where
    phi Nil = nil
    phi (Cons x (xs, xs')) | p x       = xs'
                           | otherwise = cons x xs

-- xs = cons 4 $ cons 2 $ cons 8 $ cons 3 $ cons 1 $ cons 7 $ cons 6 $ cons 5 nil


unzip :: (Bifunctor f, Functor (f a), Functor (f b), Functor (f (a, b))) =>
         Fix (f (a, b)) -> (Fix (f a), Fix (f b))
unzip = pair (map fst, map snd)

zip :: ApplicativeBifunctor f => Fix (f a) -> Fix (f b) -> Fix (f (a, b))
zip xs ys = In $ biap (bimap ((,), zip) (out xs)) (out ys)
