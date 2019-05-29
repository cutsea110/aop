module Inf where

import Prelude hiding (foldr)

data Nat = Z | S Nat deriving (Show, Eq)

foldn (c, f) = u
  where
    u Z = c
    u (S n) = f (u n)

unfoldn psi x = case psi x of
  Nothing -> Z
  Just x' -> S (unfoldn psi x')

idNat = foldn (Z, S)
coIdNat = unfoldn psi
  where
    psi Z = Nothing
    psi (S n) = Just n

toNat = unfoldn psi
  where
    psi 0 = Nothing
    psi n = Just (n-1)
fromNat = foldn (0, (+1))

infNat = unfoldn (\x -> Just x) 0

data List a = Nil | Cons a (List a) deriving (Show, Eq)

foldr (c, f) = u
  where
    u Nil = c
    u (Cons x xs) = f x (u xs)

unfoldr psi x = case psi x of
  Nothing -> Nil
  Just (x, xs) -> Cons x (unfoldr psi xs)


idList = foldr (Nil, Cons)
coIdList = unfoldr psi
  where
    psi Nil = Nothing
    psi (Cons x xs) = Just (x, xs)

toIntList = unfoldr psi
  where
    psi 0 = Nothing
    psi n = Just (n, n-1)

infList = unfoldr (\x -> Just (x, x+1)) 0
