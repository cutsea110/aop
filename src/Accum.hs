-- exercise 3.45
module Accum where

data Tree a = Tip a
            | Bin (Tree a) (Tree a)
            deriving (Eq, Show)

tip = Tip
bin = uncurry Bin

foldt f g = u
  where
    u (Tip x) = f x
    u (Bin l r) = g (u l, u r)

-- mapt f = (|[tip, bin] . F(f, id)|) = (|[tip, bin] . (f + id * id)|) = (|tip . f, bin|)
mapt f = foldt (tip . f) bin
