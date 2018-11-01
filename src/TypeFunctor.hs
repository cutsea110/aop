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
