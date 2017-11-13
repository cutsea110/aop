{-# LANGUAGE LambdaCase, Rank2Types, TypeOperators #-}
module Promorphism where

import Data.Foldable
import Prelude hiding (sum, Functor, fmap)

import FixPrime

data ListF a x = Nil | Cons a x deriving Show
type List a = Fix (ListF a)

instance Bifunctor ListF where
  bimap (f, g) Nil = Nil
  bimap (f, g) (Cons a x) = Cons (f a) (g x)

instance Functor (ListF a) where
  fmap f = bimap (id, f)


sumAlg :: Num a => ListF a a -> a
sumAlg = \case
  Cons h t -> h + t
  Nil      -> 0

lenAlg :: ListF a Int -> Int
lenAlg = \case
  Cons h t -> 1 + t
  Nil      -> 0

sum :: Num a => List a -> a
sum = cata sumAlg
len :: List a -> Int
len = cata lenAlg

smallSumAlg :: (Ord a, Num a) => ListF a a -> a
smallSumAlg = \case
  Cons h t -> if h <= 10 then h + t else 0
  Nil      -> 0
smallLenAlg :: (Ord a, Num a) => ListF a Int -> Int
smallLenAlg = \case
  Cons h t -> if h <= 10 then 1 + t else 0
  Nil      -> 0

small :: (Ord a, Num a) => ListF a b -> ListF a b
small Nil = Nil
small term@(Cons h t)
  | h <= 10   = term
  | otherwise = Nil

embed :: Bifunctor f => f t t -> t
embed = undefined

smallSum :: (Ord a, Num a) => List a -> a
smallSum = prepro small sumAlg

smallLen :: (Ord a, Num a) => List a -> Int
smallLen = prepro small lenAlg

streamCoalg :: Enum a => a -> ListF a a
streamCoalg n = Cons n (succ n)

smallStream :: (Ord a, Num a, Enum a) => a -> List a
smallStream = postpro small streamCoalg
