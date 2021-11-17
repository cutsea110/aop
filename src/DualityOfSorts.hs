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
data a :+: b = Stop a | Go b deriving Show

-- | split in paper, but we use the name split for the other function
pair :: (x -> a) -> (x -> b) -> x -> a :*: b
pair f g x = (f x, g x)
join :: (a -> x) -> (b -> x) -> (a :+: b -> x)
join f g (Stop  a) = f a
join f g (Go b) = g b

para :: Functor f => (f (Fix f :*: a) -> a) -> Fix f -> a
para f = f . fmap (pair id (para f)) . out
para' :: Functor f => (f (Fix f :*: a) -> a) -> Fix f -> a
para' f = snd . fold (pair (In . fmap fst) f)
apo :: Functor f => (a -> f (Fix f :+: a)) -> a -> Fix f
apo f = In . fmap (join id (apo f)) . f

suffixes :: Fix List -> [Fix List]
suffixes = para (suf $?)
suf :: List (Fix List :*: [Fix List]) -> [Fix List]
suf Nil               = []
suf (Cons _n (l, ls)) = l:ls

--------------------------------------------------------------------------------------
-- Section 5

type f $+$ a = a :+: f a
type f $*$ a = a :*: f a

insertSort'' :: Fix List -> Fix SList
insertSort'' = fold (apo (insert'' $?))
insert'' :: List (Fix SList) -> SList (List $+$ Fix SList)
insert'' Nil = SNil
insert'' (Cons a (In SNil)) = SCons a (Stop (In SNil))
insert'' (Cons a (In (SCons b x')))
  | a <= b    = SCons a (Stop (In (SCons b x')))
  | otherwise = SCons b (Go (Cons a x'))

swop :: List (SList $*$ x) -> SList (List $+$ x)
swop Nil = SNil
swop (Cons a (x, SNil)) = SCons a (Stop x)
swop (Cons a (x, SCons b x'))
  | a <= b    = SCons a (Stop x)
  | otherwise = SCons b (Go (Cons a x'))

insertSort' :: Fix List -> Fix SList
insertSort' = fold (apo ((swop . fmap (pair id out)) $?))
selectSort' :: Fix List -> Fix SList
selectSort' = unfold (para ((fmap (join id In) . swop) $?))

--------------------------------------------------------------------------------------
-- Section 6

mergeSort :: [Integer] -> [Integer]
mergeSort []  = []
mergeSort [a] = [a]
mergeSort as = merge (mergeSort bs) (mergeSort cs)
  where (bs, cs) = split as
split :: [Integer] -> ([Integer], [Integer])
split []       = ([], [])
split [a]      = ([a], [])
split (a:b:cs) = (a:as, b:bs)
  where (as, bs) = split cs

merge :: [Integer] -> [Integer] -> [Integer]
merge as     [] = as
merge []     bs = bs
merge (a:as) (b:bs)
  | a <= b    = a:merge as (b:bs)
  | otherwise = b:merge (a:as) bs

data Tree tree = Tip | Leaf Integer | Fork tree tree deriving Show
instance Functor Tree where
  fmap f Tip = Tip
  fmap f (Leaf a) = Leaf a
  fmap f (Fork l r) = Fork (f l) (f r)

--------------------------------------------------------------------------------------
-- Section 6.1

data List' list = Nil' | Single' Integer | Cons' Integer list deriving Show
instance Functor List' where
  fmap f Nil' = Nil'
  fmap f (Single' a) = Single' a
  fmap f (Cons' a as) = Cons' a (f as)

nil' :: Fix List'
nil' = In Nil'
single' :: Integer -> Fix List'
single' a = In (Single' a)
cons' :: Integer -> Fix List' -> Fix List'
cons' a as = In (Cons' a as)

fromList' :: Fix List' -> [Integer]
fromList' (In Nil') = []
fromList' (In (Single' a)) = [a]
fromList' (In (Cons' a as)) = a:fromList' as

sample' :: Fix List'
sample' = cons' 3 (cons' 1 (cons' 4 (single' 2)))

grow :: List' (Tree $*$ x) -> Tree (List' $+$ x)
grow Nil' = Tip
grow (Single' a) = Leaf a
grow (Cons' a (t, Tip)) = Leaf a
grow (Cons' a (t, Leaf b)) = Fork (Go (Single' a)) (Stop t)
-- grow (Cons' a (t, Fork l r)) = Fork (Go (Cons' a l)) (Stop r)
grow (Cons' a (t, Fork l r)) = Fork (Go (Cons' a r)) (Stop l)

makeTree :: Fix List' -> Fix Tree
makeTree = fold (apo (grow . fmap (pair id out)))
makeTree' :: Fix List' -> Fix Tree
makeTree' = unfold (para (fmap (join id In) . grow))

--------------------------------------------------------------------------------------
-- Section 6.2

merge' :: Tree (SList $*$ x) -> SList (Tree $+$ x)
merge' Tip = SNil
merge' (Leaf a) = SCons a (Go Tip)
merge' (Fork (l, SNil)       (r, SNil))       = SNil
merge' (Fork (l, SNil)       (r, SCons b r')) = SCons b (Stop r')
merge' (Fork (l, SCons a l') (r, SNil))       = SCons a (Stop l')
merge' (Fork (l, SCons a l') (r, SCons b r'))
  | a <= b    = SCons a (Go (Fork l' r))
  | otherwise = SCons b (Go (Fork l r'))

mergeTree :: Fix Tree -> Fix SList
mergeTree = fold (apo (merge' . fmap (pair id out)))
mergeTree' :: Fix Tree -> Fix SList
mergeTree' = unfold (para (fmap (join id In) . merge'))

--------------------------------------------------------------------------------------
-- Section 6.3

mergeSort1 :: Fix List' -> Fix SList
mergeSort1 = mergeTree . makeTree
mergeSort2 :: Fix List' -> Fix SList
mergeSort2 = mergeTree . makeTree'
mergeSort3 :: Fix List' -> Fix SList
mergeSort3 = mergeTree' . makeTree
mergeSort4 :: Fix List' -> Fix SList
mergeSort4 = mergeTree' . makeTree'
