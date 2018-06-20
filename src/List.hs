{-# LANGUAGE LambdaCase #-}
module List where

import Prelude hiding (foldr, reverse)

data Cons a = Nil
            | Cons a (Cons a)
            deriving (Show, Eq)

cross (f, g) (x, y) = (f x, g y)
compose (f, g) = f . g -- (.) f g => uncurry (.) (f, g)

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
-- ccat' Nil = id
-- ccat' (Cons x xs) = uncurry (.) (cross (Cons, id) (x, ccat' xs))
-- ccat' (Cons x xs) = compose (cross (Cons, id) (x, ccat' xs))
ccat' = foldr (id, compose . cross (Cons, id))

len = foldr (0, plus1)
  where
    plus1 (_, x) = 1+x

reverse bs = foldr (id, (\(b, g) x -> g (Cons b x))) bs Nil

para :: (b, (a, (Cons a, b)) -> b) -> Cons a -> b
para (c, g) Nil = c
para (c, g) (Cons x xs) = g (x, (xs, para (c, g) xs))

-- insert 2 $  insert 1 $ insert 4 $ insert 3 Nil
insert :: Ord a => a -> Cons a -> Cons a
insert v = para (c, g)
  where
    c = Cons v Nil
    g (x, (xs, ys)) = if v <= x then (Cons v (Cons x xs)) else Cons x ys

-- Y for List
fixL f = \case
  Nil -> Nil
  Cons x xs -> Cons x (f xs)

-- I for List
idL = fixL idL
