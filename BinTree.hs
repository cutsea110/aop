module BinTree where

data Bin a = Bin a (Bin a) (Bin a) deriving Show


cata f (Bin x l r) = f x (cata f l) (cata f r)

para g (Bin x l r) = g x (l, para g l) (r, para g r)


ana phi x = case phi x of
  (x', l, r) -> Bin x' (ana phi l) (ana phi r)
