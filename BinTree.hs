module BinTree where

data Bin a = Bin a (Bin a) (Bin a) deriving Show

cata f (Bin x l r) = f x (cata f l) (cata f r)

para f (Bin x l r) = f x (l, para f l) (r, para f r)
