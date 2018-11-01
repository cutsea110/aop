module TypeFunctor where

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

-- F(A,T) = 1 + A * T
-- F(f,g) = 1 + f * g
data List a = Nil
            | Cons a (List a)
            deriving (Show, Eq)

fold (c, f) = u
  where
    u Nil = c
    u (Cons x xs) = f (x, u xs)

nil = Nil
cons = uncurry Cons

-- list f = fold (alpha . F(f, id))
--        = fold ([nil, cons] . (id + f * id))
list f = fold (nil, cons . cross (f, id))
{--
-- F(A,T) = A + T * T
-- F(f,g) = f + g * g
data BTree a = Tip a
             | Bin (BTree a) (BTree a)
             deriving (Show, Eq)

tip = Tip
bin = uncurry Bin

foldt (f, g) = u
  where
    u (Tip a) = f a
    u (Bin l r) = g (u l, u r)

-- tree f = foldt (alpha . F(f, id))
--        = foldt ([tip, bin] . (f + id * id))
tree f = foldt (tip . f, bin . cross (id, id))
--}
data Tree a = Fork a (Forest a) deriving (Show, Eq)
data Forest a = Nulls
              | Grows (Tree a) (Forest a)
              deriving (Show, Eq)

fork = uncurry Fork
nulls = Nulls
grows = uncurry Grows

foldt (g, c, h) (Fork x fs)  = g (x, foldf (g, c, h) fs)

foldf (g, c, h) Nulls        = c
foldf (g, c, h) (Grows t fs) = h (foldt (g, c, h) t, foldf (g, c, h) fs)
