{-# LANGUAGE DeriveFunctor,
             TypeSynonymInstances,
             FlexibleInstances
#-}
module FixTest where

import Prelude hiding (sum, length, succ)
import Fix

-- | Natural Number
data NatF x = Zero | Succ x deriving (Show, Functor)

type Nat = Fix NatF

zero :: Nat
zero = In Zero
succ :: Nat -> Nat
succ n = In (Succ n)

instance Show Nat where
  show (In Zero) = "Zero"
  show (In (Succ (In Zero))) = "Succ Zero"
  show (In (Succ n)) = "Succ (" ++ show n ++ ")"

toInt :: Nat -> Int
toInt = cata phi
  where
    phi Zero = 0
    phi (Succ n) = 1 + n -- !?

double :: Nat -> Nat
double = cata phi
  where
    phi Zero = zero
    phi (Succ n) = succ (succ n) -- !?

fromInt :: Int -> Nat
fromInt = ana psi
  where
    psi n = if n <= 0 then Zero else Succ (n - 1)

plus :: Nat -> Nat -> Nat
plus x = cata phi
  where
    phi Zero = x
    phi (Succ y) = succ y

mult :: Nat -> Nat -> Nat
mult x = cata phi
  where
    phi Zero = zero
    phi (Succ y) = plus x y

expr :: Nat -> Nat -> Nat
expr x = cata phi
  where
    phi Zero = succ zero
    phi (Succ y) = mult x y

fact :: Nat -> Nat
fact = para phi
  where
    phi Zero = succ zero
    phi (Succ (n, m)) = mult (succ n) m

-- | List a

data ListF a x = Nil | Cons a x deriving (Show, Functor)

type List a = Fix (ListF a)

nil :: List a
nil = In Nil
cons :: a -> List a -> List a
cons x xs = In (Cons x xs)

instance Show a => Show (List a) where
  show x = "(" ++ show (out x) ++ ")"

sum = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = a + x -- !?

length = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = 1 + x -- !?

genList :: Integer -> List Integer
genList = ana psi
  where
    psi n = if n <= 0 then Nil else Cons n (n - 1)

-- insert 4 $ insert 2 $ insert 1 $ insert 3 nil
insert :: Ord a => a -> List a -> List a
insert v = para phi
  where
    phi Nil = cons v nil
    phi (Cons x (xs, ys)) = if v <= x then cons v (cons x xs) else cons x ys


