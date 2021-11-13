{-# LANGUAGE FlexibleInstances, TypeOperators #-}
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
instance Show (Fix List) where
  show x = show (fromList x)
instance Show (Fix SList) where
  show x = show (fromSList x)

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

nil :: Fix List
nil = In Nil
cons :: Integer -> Fix List -> Fix List
cons x xs = In (Cons x xs)
snil :: Fix SList
snil = In SNil
scons :: Integer -> Fix SList -> Fix SList
scons x xs = In (SCons x xs)

fromList :: Fix List -> [Integer]
fromList (In Nil) = []
fromList (In (Cons x xs)) = x:fromList xs

fromSList :: Fix SList -> [Integer]
fromSList (In SNil) = []
fromSList (In (SCons x xs)) = x:fromSList xs

sample :: Fix List
sample = cons 3 (cons 1 (cons 4 (cons 2 nil)))

naiveInsertSort :: Fix List -> Fix SList
naiveInsertSort = fold (unfold (naiveInsert $?))
naiveInsert :: List (Fix SList) -> SList (List (Fix SList))
naiveInsert Nil                = SNil
naiveInsert (Cons a (In SNil)) = SCons a Nil
naiveInsert (Cons a (In (SCons b x)))
  | a <= b    = SCons a (Cons b x)
  | otherwise = SCons b (Cons a x)

bubbleSort :: Fix List -> Fix SList
bubbleSort = unfold (fold (bubble $?))
bubble :: List (SList (Fix List)) -> SList (Fix List)
bubble Nil = SNil
bubble (Cons a SNil) = SCons a (In Nil)
bubble (Cons a (SCons b x))
  | a <= b    = SCons a (In (Cons b x))
  | otherwise = SCons b (In (Cons a x))



swap :: List (SList x) -> SList (List x)
swap Nil = SNil
swap (Cons a SNil) = SCons a Nil
swap (Cons a (SCons b x))
  | a <= b    = SCons a (Cons b x)
  | otherwise = SCons b (Cons a x)

naiveInsertSort' :: Fix List -> Fix SList
naiveInsertSort' = fold (unfold ((swap . fmap out) $?))
bubbleSort' :: Fix List -> Fix SList
bubbleSort' = unfold (fold ((fmap In . swap) $?))

--------------------------------------------------------------------------------------
-- Section 4

type a :*: b = (a, b)
type a :+: b = Either a b

split :: (x -> a) -> (x -> b) -> x -> a :*: b
split f g x = (f x, g x)
join :: (a -> x) -> (b -> x) -> (a :+: b -> x)
join f g (Left  a) = f a
join f g (Right b) = g b

para :: Functor f => (f (Fix f :*: a) -> a) -> Fix f -> a
para f = f . fmap (split id (para f)) . out
para' :: Functor f => (f (Fix f :*: a) -> a) -> Fix f -> a
para' f = snd . fold (split (In . fmap fst) f)
apo :: Functor f => (a -> f (Fix f :+: a)) -> a -> Fix f
apo f = In . fmap (join id (apo f)) . f
