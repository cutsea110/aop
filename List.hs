module List where

import Prelude hiding (foldr)

data Cons a = Nil
            | Cons a (Cons a)
            deriving (Show, Eq)
foldr (c, f) Nil = c
foldr (c, f) (Cons x xs) = f x (foldr (c, f) xs)

unfoldr phi x = case phi x of
  Nothing -> Nil
  Just (a, x') -> Cons a (unfoldr phi x')

gen = unfoldr phi
  where
    phi n = if n <= 0 then Nothing else Just (n, n-1)

listr f Nil = Nil
listr f (Cons x xs) = Cons (f x) (listr f xs)

listr' f = foldr (Nil, Cons <$> f <$> id)

cat Nil ys         = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

ccat Nil = id
ccat (Cons a x) = (Cons a . ccat x)

-- ccat' = foldr (id, (.) <$> Cons <$> id)
ccat' = foldr (id, compose (Cons, id))
  where
    compose (f, g) x y = f x . g y

len = foldr (0, \_ y -> (1+y))
