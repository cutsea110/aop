module ShortPathSearch where

import FixPrime

-- | Tree
data TreeF a x = Tip a | Bin (a, x) (a, x) deriving Show
type Tree a = Fix (TreeF a)

tip :: a -> Tree a
tip = In . Tip

bin :: (a, Tree a) -> (a, Tree a) -> Tree a
bin (x, l) (y, r) = In (Bin (x, l) (y, r))
