module Inf where

import Prelude hiding (foldr)

data Nat = Z | S Nat deriving (Show, Eq)

foldn (c, f) = mu
  where
    mu Z = c
    mu (S n) = f (mu n)

unfoldn psi = nu
  where
    nu x = case psi x of
      Nothing -> Z
      Just x' -> S (nu x')

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

foldr (c, f) = mu
  where
    mu Nil = c
    mu (Cons x xs) = f x (mu xs)

unfoldr psi = nu
  where
    nu x = case psi x of
      Nothing -> Nil
      Just (x, xs) -> Cons x (nu xs)


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

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

foldt (f, g) = mu
  where
    mu (Leaf x) = f x
    mu (Node l r) = g (mu l) (mu r)

unfoldt psi = nu
  where
    nu x = case psi x of
      Left x' -> Leaf x'
      Right (l, r) -> Node (nu l) (nu r)

idTree = foldt (Leaf, Node)
coIdTree = unfoldt psi
  where
    psi (Leaf x) = Left x
    psi (Node l r) = Right (l, r)

infTree = unfoldt (\x -> Right (x, x+1)) 0

infTree' = unfoldt (\x -> if x <= 0 then Left x else Right (x-1, x+1)) 1
