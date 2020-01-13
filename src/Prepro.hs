{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Prepro where

import Data.Functor.Foldable
import Prelude hiding (sum)

sumAlg :: Num a => ListF a a -> a
sumAlg = \case
  Cons h t -> h + t
  Nil      -> 0
lenAlg :: ListF a Int -> Int
lenAlg = \case
  Cons _ t -> 1 + t
  Nil      -> 0

small :: (Ord a, Num a) =>  ListF a b -> ListF a b
small Nil = Nil
small term@(Cons h t) | h <= 10   = term
                      | otherwise = Nil

sum :: Num a => [a] -> a
sum = cata sumAlg

len :: [a] -> Int
len = cata lenAlg

smallSum :: (Ord a, Num a) => [a] -> a
smallSum = prepro small sumAlg

smallLen :: (Ord a, Num a) => [a] -> Int
smallLen = prepro small lenAlg

