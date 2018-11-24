module Combinatorial where
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
        f = cat . pair (list cons . cpr, outr)

cplist :: [[a]] -> [[a]]
cplist = cata (wrap nil, list cons . cpp)

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
