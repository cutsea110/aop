{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}
module Bernoulli where

import Prelude hiding (Functor, fmap, succ, cons, nil, subtract)
import Data.Ratio


import Box100 (calc)
import FixPrime hiding (map)

-- https://repl.it/@lotz84/AjarCelebratedTask

data NatF a = Z | S a deriving Show
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

toNat :: (Ord t, Num t) => t -> Nat
toNat n = if n <= 0 then zero else succ (toNat (n-1))
fromNat :: Num p => Nat -> p
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

toList :: Cofree NatF a -> List a
toList (Cf (In (Hisx (a, Z))))    = cons a nil
toList (Cf (In (Hisx (a, S xs)))) = cons a (toList (Cf xs))

instance Bifunctor ListF where
  bimap (f, g) Nil = Nil
  bimap (f, g) (Cons x y) = Cons (f x) (g y)

instance Functor (ListF a) where
  fmap f = bimap (id, f)

fact :: Integer -> Integer
fact = para phi . toNat
  where
    phi Z = 1
    phi (S (r, n)) = (1 + fromNat r) * n

comb :: Integer -> Integer -> Integer
comb n k = fact n `div` (fact (n-k) * fact k)

lenAlg :: Num a => ListF t a -> a
lenAlg Nil = 0
lenAlg (Cons _ r) = r + 1

-- lotz's solution by using zygo.
bernoulli' :: Nat -> Ratio Integer
bernoulli' = histo phi
  where
    phi Z = 1 % 1
    phi (S r) = (-1) % (n + 1) * zygo lenAlg g rs
      where
        rs = toList r
        n = cata lenAlg rs
        g Nil = 0
        g (Cons bn (k, r)) = r + bn * fromIntegral ((n+1) `comb` k)

genIndex :: (Num a, Enum a) => a -> [(a, a)]
genIndex n = [(x, n - x) | x <- [0..n]]

pascal'sTriangle :: [[Integer]]
pascal'sTriangle = let ones = 1:ones in ones:(map (1:) (calc (ones, ones)))

combs :: Integer -> [Integer]
combs = map f . genIndex . fromIntegral where f (x, y) = fromInteger (pascal'sTriangle !! x !! y)

-- my solution to answers between 0 to n at a time.
bernoulli :: Nat -> [Ratio Integer]
bernoulli = snd . histo phi
  where
    phi Z = (0, [1])
    phi (S r) = (n + 1, bs ++ [bn])
      where
        (n, bs) = extract r
        ts = map fromInteger $ init $ combs (n + 2)
        bn = (fromInteger (n + 2) - sum (zipWith (*) ts bs)) * recip (ts !! fromInteger (n + 1))

pp :: Integer -> IO ()
pp = mapM_ print . bernoulli . toNat

main :: IO ()
main = pp 1000
