module TypeFunctor where

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

-- F(A,T) = 1 + A * T
-- F(f,g) = 1 + f * g
data List a = Nil
            | Cons a (List a)
            deriving (Show, Eq)

cata (c, f) = u
  where
    u Nil = c
    u (Cons x xs) = f (x, u xs)

nil = Nil
cons = uncurry Cons

-- listr f = cata (alpha . F(f, id))
--         = cata ([nil, cons] . (id + f * id))
listr f = cata (nil, cons . cross (f, id))
