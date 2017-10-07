module List where

import Prelude hiding (foldr)

data Cons a = Nil
            | Cons a (Cons a)
            deriving (Show, Eq)

cross (f, g) (x, y) = (f x, g y)
compose (f, g) = f . g

nil = Nil
cons = uncurry Cons

foldr (c, f) Nil = c
foldr (c, f) (Cons x xs) = f (x, foldr (c, f) xs)

unfoldr phi x = case phi x of
  Nothing -> nil
  Just (a, x') -> cons (a, unfoldr phi x')

gen = unfoldr phi
  where
    phi n = if n <= 0 then Nothing else Just (n, n-1)

listr f Nil = nil
listr f (Cons x xs) = cons (f x, listr f xs)

listr' f = foldr (Nil, cons . cross (f, id))

cat (Nil, ys)       = ys
cat (Cons x xs, ys) = cons (x, cat (xs, ys))

ccat Nil = id
ccat (Cons a x) = (Cons a . ccat x)

-- ccat' = foldr (id, (.) <$> Cons <$> id)
ccat' = foldr (id, compose . cross (Cons, id))

len = foldr (0, \(_, y) -> (1+y))
