{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Fix where

import Prelude hiding (sum ,length, succ)

newtype Fix f = In { out :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out

ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi

-- | Natural Number
data NatF x = Zero | Succ x deriving (Show, Functor)

type Nat = Fix NatF

zero :: Nat
zero = In Zero
succ :: Nat -> Nat
succ n = In (Succ n)

instance Show Nat where
  show n = "(" ++ show (out n) ++ ")"

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
