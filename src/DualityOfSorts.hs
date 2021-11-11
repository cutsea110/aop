module DualityOfSorts where
-- ref.) Duality of Sorts by Rafl Hinze

import Debug.Trace (trace)
import Data.List (delete, unfoldr)

--------------------------------------------------------------------------------------
debug = True

tracer :: String -> a -> a
tracer = if debug then trace else const id

($?) :: (Show a, Show b) => (a -> b) -> a -> b
f $? x = let v = f x
             msg = " {- " ++ show x ++ " => " ++ show v ++ " -}"
         in tracer msg v
--------------------------------------------------------------------------------------
-- Section 1

insertSort :: [Integer] -> [Integer]
insertSort = foldr (curry (insert $?)) []

insert :: (Integer, [Integer]) -> [Integer]
insert (y, ys) = xs ++ [y] ++ zs
  where (xs, zs) = span (<=y) ys

selectSort :: [Integer] -> [Integer]
selectSort = unfoldr (select $?)

select :: [Integer] -> Maybe (Integer, [Integer])
select [] = Nothing
select xs = Just (x, xs')
  where x   = minimum xs
        xs' = delete x xs

--------------------------------------------------------------------------------------
-- Section 2

data List list = Nil | Cons Integer list deriving Show
newtype Fix f = In { out :: f (Fix f) }

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons k x) = Cons k (f x)

fold :: Functor f => (f a -> a) -> Fix f -> a
fold a = a . fmap (fold a) . out

unfold :: Functor f => (a -> f a) -> a -> Fix f
unfold c = In . fmap (unfold c) . c

--------------------------------------------------------------------------------------
-- Section 3

data SList list = SNil | SCons Integer list deriving Show
instance Functor SList where
  fmap f SNil = SNil
  fmap f (SCons k list) = SCons k (f list)
