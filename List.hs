module List where

import Prelude hiding (foldr)

data Cons a = Nil
            | Cons a (Cons a)
            deriving (Show, Eq)

cross (f, g) (x, y) = (f x, g y)
compose (f, g) = f . g

nil = Nil
cons = uncurry Cons

foldr :: (b, (a, b) -> b) -> Cons a -> b
foldr (c, f) Nil = c
foldr (c, f) (Cons x xs) = f (x, foldr (c, f) xs)

cfoldr :: (b, a -> b -> b) -> Cons a -> b
cfoldr (c, f) Nil = c
cfoldr (c, f) (Cons x xs) = f x (cfoldr (c, f) xs)

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

ccat :: Cons a -> Cons a -> Cons a
ccat Nil = id
ccat (Cons a x) = (Cons a . ccat x)

ccat' :: Cons a -> Cons a -> Cons a
ccat' Nil = id
ccat' (Cons x xs) = compose (cross (Cons , ccat') (x, xs))

len = foldr (0, plus1)
  where
    plus1 (_, x) = 1+x
