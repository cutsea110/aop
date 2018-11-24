module Combinatorial where

data List a = Nil | Cons a (List a) deriving (Show, Eq)

cata (c, f) Nil = c
cata (c, f) (Cons x xs) = f (x, cata (c, f) xs)

listr f Nil = Nil
listr f (Cons x xs) = Cons (f x) (listr f xs)
