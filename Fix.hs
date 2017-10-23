{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Fix where

import Prelude hiding (sum ,length, succ)

newtype Fix f = In { out :: f (Fix f) }

cata :: Functor f => (f b -> b) -> Fix f -> b
cata phi = phi . fmap (cata phi) . out

-- | Natural Number
data NatF x = Zero | Succ x deriving (Show, Functor)

type Nat = Fix NatF

instance Show Nat where
  show n = "(" ++ show (out n) ++ ")"


zero :: Nat
zero = In Zero
succ :: Nat -> Nat
succ n = In (Succ n)

toInt :: Nat -> Int
toInt = cata phi
  where
    phi Zero = 0
    phi (Succ n) = 1 + n

double :: Nat -> Nat
double = cata phi
  where
    phi Zero = zero
    phi (Succ n) = succ (succ n)

-- | List a

data ListF a x = Nil | Cons a x deriving (Show, Functor)

type List a = Fix (ListF a)

instance Show a => Show (List a) where
  show x = "(" ++ show (out x) ++ ")"

nil :: List a
nil = In Nil
cons :: a -> List a -> List a
cons x xs = In (Cons x xs)

sum = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = a + x -- !?

length = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = 1 + x -- !?
