{-# LANGUAGE FlexibleContexts #-}
module Combinatorial where

import Data.List ((\\), intersect, union)
import Prelude hiding (concat,elem)

cata :: (b, (a, b) -> b) -> [a] -> b
cata (c, f) = foldr (curry f) c
outl :: (a, b) -> a
outl (x, _) = x
outr :: (a, b) -> b
outr (_, y) = y
cpp :: ([a], [b]) -> [(a, b)]
cpp (x, y) = [(a, b) | a <- x, b <- y]
cpl :: ([a], b) -> [(a, b)]
cpl (x, b) = [(a, b) | a <- x]
cpr :: (a, [b]) -> [(a, b)]
cpr (a, x) = [(a, b) | b <- x]
wrap :: a -> [a]
wrap x = [x]
nil :: [a]
nil = []
cat :: ([a], [a]) -> [a]
cat (x, y) = x ++ y
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)
cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) (x, y) = (f x, g y)
cons :: (a, [a]) -> [a]
cons (x, xs) = x : xs
list :: (a -> b) -> [a] -> [b]
list = map

subseqs :: [a] -> [[a]]
subseqs = cata (e, f)
    where
        e = wrap nil
        f :: (a, [[a]]) -> [[a]]
        f = cat . pair (list cons . cpr, outr)

cplist :: [[a]] -> [[a]]
cplist = cata (e, f)
    where
        e = wrap nil
        f :: ([a], [[a]]) -> [[a]]
        f = list cons . cpp

inits :: [a] -> [[a]]
inits = cata (e, f)
    where
        e = wrap nil
        f :: (a, [[a]]) -> [[a]]
        f = cat . pair (const (wrap nil), list cons . cpr)

tails :: [a] -> [[a]]
tails = cata (e, f)
    where
        e = wrap nil
        f :: (a, [[a]]) -> [[a]] 
        f (a, x:xs) = (a:x):x:xs

concat :: [[a]] -> [a]
concat = cata (nil, cat)
new :: (a, [[a]]) -> [[a]]
new = cons . cross (wrap, id)
glues :: (a, [[a]]) -> [[[a]]]
glues (a, []) = []
glues (a, x:xs) = [(a:x):xs]

partitions :: [a] -> [[[a]]]
partitions = cata (e, f)
    where
        e = wrap nil
        f = concat . list (cons . pair (new, glues)) . cpr

splits :: [a] -> [([a], [a])]
splits = uncurry zip . pair (inits, tails)

adds :: (a, [a]) -> [[a]]
adds (a, x) = [y ++ [a] ++ z | (y, z) <- splits x]

perms :: [a] -> [[a]]
perms = cata (e, f)
    where
        e = wrap nil
        f = concat . list adds . cpr

consl :: (a, ([a], b)) -> ([a], b)
consl (a, (x, y)) = (a:x, y)
consr :: (a, (b, [a])) -> (b, [a])
consr (a, (x, y)) = (x, a:y)
conv :: (a, a) -> [a]
conv (l, r) = [l, r]

interleave :: [a] -> [([a],[a])]
interleave = cata (e, f)
    where
        e = wrap (nil, nil)
        f = concat . list (conv . pair (consl, consr)) . cpr

isEqual :: Eq a => [a] -> [a] -> Bool
xs `isEqual` ys = null (xs \\ ys) && null (ys \\ xs)

elem :: Eq a => [a] -> [[a]] -> Bool
elem x = cata (e, f)
    where
        e = False
        f (y,b) = b || y `isEqual` x



openSets :: Eq a => [a] -> [[[a]]]
openSets u = filter isOpen $ candidates u
    where
        candidates u = map (\x -> u:(x ++ [[]])) $ subseqs u'
            where
                u' = filter (\x -> not (null x || x == u)) $ subseqs u

        isOpen xs = all pred [(x,y) | x <- xs, y <- xs \\ [x]]
            where
                pred (x, y) = (x `intersect` y) `elem` xs && (x `union` y) `elem` xs

putOpenSets :: (Show a, Eq a) => [a] -> IO ()
putOpenSets u = mapM_ go $ zip [1..] (openSets u)
    where
        go (i, ln) = putStrLn $ show i ++ " : " ++ show ln

excer15_1 = openSets [0,1,2]

putExcer15_1 = putOpenSets [0,1,2]
