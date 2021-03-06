{-# LANGUAGE LambdaCase, Rank2Types, TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
-- ref.) https://jtobin.io/promorphisms-pre-post
module Promorphism where

import Prelude hiding (sum, Functor, fmap, map)

import Data.Bool (bool)
import FixPrime

data ListF a x = Nil | Cons a x deriving Show
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

genList :: Integer -> List Integer
genList = ana psi
  where
    psi n = if n <= 0 then Nil else Cons n (n - 1)

idAlg :: ListF a (List a) -> List a
idAlg = \case
  Cons h t -> cons h t
  Nil      -> nil

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
  Cons h t -> if h <= 10 then h + t else t
  Nil      -> 0
smallLenAlg :: (Ord a, Num a) => ListF a a -> a
smallLenAlg = \case
  Cons h t -> if h <= 10 then 1 + t else t
  Nil      -> 0

-- natural transform. Cons 9 :~> Cons 9, Cons 10 :~> Cons 10, Cons 11 :~> Nil and so on.
small :: (Ord a, Num a) => ListF a :~> ListF a
small = \case
  Cons h t | h <= 10   -> Cons h t
           | otherwise -> Nil
  Nil                  -> Nil

takeTo :: (a -> Bool) -> ListF a :~> ListF a
takeTo pred = \case
  Cons h t | pred h    -> Cons h t
           | otherwise -> Nil
  Nil                  -> Nil


-- prepro double sumAlg $ cons 1 nil => 1
--    1 + (0 * 2) => 1
-- prepro double sumAlg $ cons 1 (cons 2 nil) => 5
--    1 + (2 * 2) + (0 * 2 * 2) => 5
-- prepro double sumAlg $ cons 1 (cons 2 (cons 3 nil)) => 17
--    1 + (2 * 2) + (3 * 2 * 2) + (0 * 2 * 2 * 2) => 17
-- 
-- cata (In . double) $ cons 2 nil => Cons 4 Nil
-- cata (In . double) $ cons 2 (cons 3 nil) => Cons 4 (Cons 6 Nil)
-- cata (In . double) $ cons 2 (cons 3 (cons 4 nil)) => Cons 4 (Cons 6 (Cons 8 Nil))
--
--            
--                  ψx <= double
--  X      F(X) ---------> G(X)
--  |       |                |
-- f|   F(f)|                |G(f)
--  |       |                |
--  v       v                v
--  Y      F(Y) ---------> G(Y)
--                  ψy <= double
--
double :: Num a => ListF a x -> ListF a x
double = bimap ((*2),id)

smallSum :: (Ord a, Num a) => List a -> a
smallSum = prepro small sumAlg

smallLen :: (Ord a, Num a) => List a -> Int
smallLen = prepro small lenAlg

streamCoalg :: Enum a => a -> ListF a a
streamCoalg n = Cons n (succ n)

smallStream :: (Ord a, Num a, Enum a) => a -> List a
smallStream = postpro small streamCoalg

range :: (Enum a, Ord a, Num a) => a -> a -> List a
range from to = postpro (takeTo (<(to+1))) streamCoalg from
