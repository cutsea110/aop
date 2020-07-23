{-# LANGUAGE LambdaCase #-}
module List where

import Prelude hiding (foldr, reverse, sum)

data Cons a = Nil
            | Cons a (Cons a)
            deriving (Show, Eq)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
compose (f, g) = f . g -- (.) f g => uncurry (.) (f, g)
outl (x, _) = x
outr (_, y) = y

nil = Nil
cons = uncurry Cons

foldr :: (b, (a, b) -> b) -> Cons a -> b
foldr (c, f) = u
  where u Nil = c
        u (Cons x xs) = f (x, u xs)

cfoldr :: (b, a -> b -> b) -> Cons a -> b
cfoldr (c, f) = u
  where u Nil = c
        u (Cons x xs) = f x (u xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> Cons a
unfoldr phi x = case phi x of
  Nothing -> nil
  Just (a, x') -> cons (a, unfoldr phi x')

gen = unfoldr phi
  where
    phi n = if n <= 0 then Nothing else Just (n, n-1)

listr f = u
  where u Nil = nil
        u (Cons x xs) = cons (f x, u xs)

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
para (c, g) = u
  where u Nil = c
        u (Cons x xs) = g (x, (xs, u xs))

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

instance Functor Cons where
  fmap = listr

instance Applicative Cons where
  pure = eta
  Cons f fs <*> xxs = ccat (fmap f xxs) (fs <*> xxs)
  Nil <*> _ = Nil

instance Monad Cons where
  return = eta
  m >>= f = mu (fmap f m)

-- bad implementation
-- eta = cons . pair (id, eta)
-- 
-- TA <-------------- 1 + A x TA
--  ^                   ^
--  |                   | inr
--  |                   A x TA
--  |                   ^
--  |                   | <id, eta> or <id, const nil>
--  +-------------------A
-- eta = [nil, cons] . inr . <id, eta>
--        OR
-- eta = [nil, cons] . inr . <id, const nil>
--
eta = cons . pair (id, const nil)
mu = foldr (nil, cat)

sum = foldr (0, uncurry (+))

steepNaive Nil = True
steepNaive (Cons a x) = a > sum x && steepNaive x

genSteepList :: Integer -> Cons Integer
genSteepList = fmap (2^) . gen

steep = outl . foldr (c, f)
  where
    c = (True, 0)
    f (a, (b, x)) = (a > x && b, a + x)
