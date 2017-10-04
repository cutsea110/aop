module NonEmptyList where

import Prelude hiding (foldr, unfoldr, sum, length, map)

-- L = 1 + A x L
data List a = Nil | Cons a (List a) deriving Show

foldr :: (b, a -> b -> b) -> List a -> b
foldr (c, f) Nil = c
foldr (c, f) (Cons x xs) = f x (foldr (c, f) xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> List a
unfoldr phi xs = case phi xs of
  Nothing -> Nil
  Just (x, xs') -> Cons x (unfoldr phi xs')

-- Type functor
listr :: (a -> b) -> List a -> List b
-- listr f = foldr (Nil, Cons <$> f <$> id)
listr f = foldr (Nil, curry (uncurry Cons . cross (f, id)))
  where
    cross (f, g) (x, y) = (f x, g y)

-- Identity
cataId = foldr (Nil, Cons)
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

foldr' :: (t, a -> t -> t, a -> t -> t') -> NonEmptyList a -> t'
foldr' (c, f, g) (Pair x xs) = g x (foldr (c, f) xs)

unfoldr' :: (b -> Maybe (a, b), t -> (a, b)) -> t -> NonEmptyList a
unfoldr' (phi, psi) xs = case psi xs of
  (x, xs') -> Pair x (unfoldr phi xs')

listr' :: (a -> b) -> NonEmptyList a -> NonEmptyList b
-- listr' f = foldr' (Nil, Cons <$> f <$> id, Pair <$> f <$> id)
listr' f = foldr' (Nil, curry (uncurry Cons . cross (f, id)), curry (uncurry Pair . cross (f, id)))
  where
    cross (f, g) (x, y) = (f x, g y)

-- Identity
cataId' = foldr' (Nil, Cons, Pair)
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
length = foldr (0, const (1+))
sum :: Num a => List a -> a
sum = foldr (0, (+))

length' :: NonEmptyList a -> Int
length' = foldr' (0, const (1+), const (1+))
sum' :: Num a => NonEmptyList a -> a
sum' = foldr' (0, (+), (+))
