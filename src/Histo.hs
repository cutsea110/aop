module Histo where

import Prelude hiding (Functor, fmap, succ, cons, nil, subtract)
import FixPrime

data NatF x = Z | S x deriving Show
type Nat = Fix NatF

zero :: Nat
zero = In Z
succ :: Nat -> Nat
succ n = In (S n)

instance Functor NatF where
  fmap f Z     = Z
  fmap f (S x) = S (f x)

instance Show Nat where
  show (In Z) = "Z"
  show (In (S (In Z))) = "S Z"
  show (In (S n)) = "S (" ++ show n ++ ")"

toNat :: Int -> Nat
toNat n = if n <= 0 then zero else succ (toNat (n-1))
fromNat :: Nat -> Int
fromNat (In Z) = 0
fromNat (In (S n)) = 1 + fromNat n

data ListF a x = Nil | Cons a x deriving Show
type List a = Fix (ListF a)

instance Show a => Show (List a) where
  show (In Nil) = "Nil"
  show (In (Cons h t@(In Nil))) = "Cons " ++ show h ++ " " ++ show t
  show (In (Cons h t)) = "Cons " ++ show h ++ " (" ++ show t ++ ")"

nil :: List a
nil = In Nil
cons :: a -> List a -> List a
cons x xs = In (Cons x xs)

instance Bifunctor ListF where
  bimap (f, g) Nil = Nil
  bimap (f, g) (Cons x y) = Cons (f x) (g y)

instance Functor (ListF Nat) where
  fmap f = bimap (id, f)

-- >>> fib (toNat 100)
-- 354224848179261915075
-- >>> Prelude.map (fib . toNat) [1..100]
-- [1,1,2,3,5,8...]
fib' :: Num t => Nat -> t
fib' = histo phi
  where
    phi :: Num t => NatF (Cofree NatF t) -> t
    phi Z = 0
    phi (S n) = f1 n + f2 n
      where
        f1 :: Cofree NatF t -> t
        f1 n = extract n
        f2 :: Num t => Cofree NatF t -> t
        f2 n = case sub n of
          Z -> 1
          S y -> extract y

fib :: Int -> Integer
fib = fib' . toNat

-- ackermann function
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))
