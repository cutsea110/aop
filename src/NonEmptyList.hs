{-# LANGUAGE LambdaCase #-}
module NonEmptyList where

import Prelude hiding (foldr, unfoldr, sum, length, map, concat)

-- Utility functions as uncurried version
cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) (x, y) = (f x, g y)
compose :: (b -> c, a -> b) -> a -> c
compose (h, k) = h . k
plus1 (_, m) = 1 + m
plus (n, m) = n + m

-- L = 1 + A x L
data List a = Nil | Cons a (List a) deriving Show

nil :: () -> List a
nil () = Nil
cons = uncurry Cons

foldr :: (b, (a, b) -> b) -> List a -> b
foldr (c, f) Nil = c
foldr (c, f) (Cons x xs) = f (x, (foldr (c, f) xs))

unfoldr :: (b -> Maybe (a, b)) -> b -> List a
unfoldr phi xs = case phi xs of
  Nothing -> nil ()
  Just (x, xs') -> cons (x, (unfoldr phi xs'))

-- Type functor
listr :: (a -> b) -> List a -> List b
-- listr f = foldr (Nil, Cons <$> f <$> id)
listr f = foldr (nil (), (cons . cross (f, id)))

-- Identity
cataId = foldr (nil (), cons)
anaId = unfoldr phi
  where
    phi Nil = Nothing
    phi (Cons x xs) = Just (x, xs)

-- Utility
gen n = unfoldr phi n
  where
    phi n = if n <= 0 then Nothing else Just (n, n-1)

----
-- L^{+} = 1 + A x L
data NonEmptyList a = Pair a (List a) deriving Show

pair = uncurry Pair

foldr' :: (t, (a, t) -> t, (a, t) -> t') -> NonEmptyList a -> t'
foldr' (c, f, g) (Pair x xs) = g (x, (foldr (c, f) xs))

unfoldr' :: (b -> Maybe (a, b), t -> (a, b)) -> t -> NonEmptyList a
unfoldr' (phi, psi) xs = case psi xs of
  (x, xs') -> Pair x (unfoldr phi xs')

listr' :: (a -> b) -> NonEmptyList a -> NonEmptyList b
-- listr' f = foldr' (nil, Cons <$> f <$> id, Pair <$> f <$> id)
listr' f = foldr' (nil (), (cons . cross (f, id)), (pair . cross (f, id)))

-- Identity
cataId' = foldr' (nil (), cons, Pair)
anaId' = unfoldr' (phi, psi)
  where
    phi Nil = Nothing
    phi (Cons x xs) = Just (x, xs)
    psi (Pair x xs) = (x, xs)

-- Utility
gen' n = unfoldr' (phi, psi) n
  where
    phi n = if n <= 0 then Nothing else Just (n, n-1)
    psi n = (n, n-1)

------------------

length :: List a -> Int
length = foldr (0, plus1)

sum :: Num a => List a -> a
sum = foldr (0, plus)

length' :: NonEmptyList a -> Int
length' = foldr' (0, plus1, plus1)

sum' :: Num a => NonEmptyList a -> a
sum' = foldr' (0, plus, plus)

length'' (Pair x xs) = 1 + length xs
sum'' (Pair x xs) = x + sum xs

wrap :: a -> NonEmptyList a
wrap x = Pair x Nil

cons' :: (a, NonEmptyList a) -> NonEmptyList a
cons' (x, Pair y ys) = pair (x, cons (y, ys))

head' :: NonEmptyList a -> a
head' (Pair x _) = x

tail' :: NonEmptyList a -> List a
tail' (Pair _ xs) = xs

toList :: NonEmptyList a -> List a
toList (Pair x xs) = cons (x, xs)

fromList :: List a -> NonEmptyList a
fromList (Cons x xs) = pair (x, xs)

cat :: List a -> List a -> List a
-- cat Nil ys = ys
-- cat (Cons x xs) ys = Cons x (cat xs ys)
-- 
-- cat Nil = id
-- cat (Cons x xs) = Cons x . cat xs

cat = foldr (id, compose . cross (Cons, id))

cat' :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
-- cat' (Pair x Nil) ys = cons' (x, ys)
-- cat' (Pair x xs) ys = cons' (x, (cat' (fromList xs) ys))
--
-- cat' (Pair x Nil) = curry cons' x
-- cat' (Pair x xs) = curry cons' x . cat' (fromList xs)
cat' = foldr' (id, compose . cross (Cons, id), uncurry g) -- How?
  where
    g :: a -> (List a -> List a) -> NonEmptyList a -> NonEmptyList a
    g x f = Pair x . f . toList
  

paralist (d, g) Nil = d
paralist (d, g) (Cons x xs) = g x (xs, paralist (d, g) xs)

paralist' (c, f, g) (Pair x xs) = g x (xs, paralist (c, f) xs)

-- Y for List
fixL f = \case
  Nil -> Nil
  Cons x xs -> Cons x (f xs)

-- I for List
idL = fixL idL

fixNL f = \case
  Pair x xs -> Pair x (f xs)

idNL = fixNL idL
