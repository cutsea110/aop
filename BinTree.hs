module BinTree where

data Bin a = Leaf | Bin a (Bin a) (Bin a) deriving Show

cata (d, g) Leaf = d
cata (d, g) (Bin x l r) = g x (cata (d, g) l) (cata (d, g) r)

para (d, g) Leaf = d
para (d, g) (Bin x l r) = g x (l, para (d, g) l) (r, para (d, g) r)


ana phi x = case phi x of
  Nothing -> Leaf
  Just (x', l, r) -> Bin x' (ana phi l) (ana phi r)

gen = ana phi
  where
    phi n = if n <= 1 then Nothing else Just (n, n-1, n-2)
    
