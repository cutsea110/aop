{-# LANGUAGE FlexibleContexts #-}
module Combinatorial where

import Data.List ((\\), intersect, union)
import Prelude hiding (concat,elem)

{--
subseqs :: [a] -> [[a]]
subseqs = foldr f e
    where
        e = [[]]
        f a xs = [a:x | x <- xs] ++ xs
--}

cata (c, f) = foldr (curry f) c
outl (x, _) = x
outr (_, y) = y
cpp (x, y) = [(a, b) | a <- x, b <- y]
cpl (x, b) = [(a, b) | a <- x]
cpr (a, x) = [(a, b) | b <- x]
wrap x = [x]
nil = []
cat (x, y) = x ++ y
pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
cons (x, xs) = x : xs
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
        f (a, (x:xs)) = [[a] ++ x] ++ [x] ++ xs

concat = cata (nil, cat)
new = cons . cross (wrap, id)
glues (a, []) = []
glues (a, x:xs) = [[[a] ++ x] ++ xs]

partitions :: [a] -> [[[a]]]
partitions = cata (e, f)
    where
        e = wrap nil
        f = concat . list (cons . pair (new, glues)) . cpr

splits = uncurry zip . pair (inits, tails)

adds (a, x) = [y ++ [a] ++ z | (y, z) <- splits x]

perms :: [a] -> [[a]]
perms = cata (e, f)
    where
        e = wrap nil
        f = concat . list adds . cpr

consl (a, (x, y)) = (a:x, y)
consr (a, (x, y)) = (x, a:y)
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
