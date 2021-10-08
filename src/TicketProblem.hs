-- | ref.) https://www.ipsj.or.jp/07editj/promenade/4605.pdf
module TicketProblem where

import Data.Char (ord)
import Data.List (intersperse, nub, group, sort)

type Rat = (Int, Int)
data Term = Val Char | App Char Term Term

trees :: [Char] -> [Char] -> [(Rat, Term)]
trees ds os = [ t | (_, t) <- vtrees ds os ]

vtrees :: [Char] -> [Char] -> [([Char], (Rat, Term))]
vtrees [c] os = [(os, (ctor c, Val c))]
vtrees ds  os = concat [ odtree os xs ys | (xs, ys) <- splits ds ]

odtree :: [Char] -> [Char] -> [Char] -> [([Char], (Rat, Term))]
odtree os ls rs
  = [ (os'', (ctoo o u v, App o l r))
    | (o:os', (u, l)) <- vtrees ls os
    , (os'' , (v, r)) <- vtrees rs os'
    ]

splits1 :: [Char] -> [([Char], [Char])]
splits1 [x]    = []
splits1 (x:xs) = ([x], xs) : [ (x:ys, zs) | (ys, zs) <- splits1 xs ]

instance Show Term where
  show (Val c) = [c]
  show (App o l r) = "(" ++ show l ++ [o] ++ show r ++ ")"

eval :: Term -> Rat
eval (Val x)     = ctor x
eval (App o l r) = ctoo o (eval l) (eval r)

ctor :: Char -> Rat
ctor x = (ord x - ord '0', 1)

ctoo :: Char -> (Rat -> Rat -> Rat)
ctoo '+' (x, y) (z, w) = (x*w+z*y, y*w)
ctoo '-' (x, y) (z, w) = (x*w-z*y, y*w)
ctoo '*' (x, y) (z, w) = (x*z, y*w)
ctoo '/' (x, y) (z, w) = if z == 0 then (0, 0) else (x*w, y*z)

ticket :: Int -> [Char] -> Term
ticket n ds = case filter (same n) (allterms ds) of
  (_, s):_ -> s
  []  -> error ("Cannot make "++show n++" with "++intersperse ',' ds++".")

same :: Int -> ((Rat, Term) -> Bool)
same i ((n, d), _) = i*d == n && d /= 0

allterms :: [Char] -> [(Rat, Term)]
allterms ds = concat [ trees ns os | ns <- perm ds, os <- rperm ops4 (length ds - 1) ]

ops4 = "+-*/"

gperm :: Eq a => [a] -> [[a]]
gperm = nub . perm

gperm' :: Ord a => [a] -> [[a]]
gperm' xs = foldr (concatMap . merges) [[]] (group (sort xs))

merges :: [a] -> [a] -> [[a]]
merges [] ys = [ys]
merges xs [] = [xs]
merges xxs@(x:xs) yys@(y:ys)
  = map (x:) (merges xs yys) ++ map (y:) (merges xxs ys)

perm [] = [[]]
perm xs = concat [ pm hs ts | (hs, ts) <- splits xs ]
  where pm _  []     = []
        pm hs (t:ts) = [ t:ys | ys <- perm (hs ++ ts) ]

splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : [ (x:ys, zs) | (ys, zs) <- splits xs ]

rperm _  0 = [[]]
rperm [] _ = []
rperm xs n = [ x:ys | x <- xs, ys <- rperm xs (n-1) ]
