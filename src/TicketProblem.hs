-- | ref.) https://www.ipsj.or.jp/07editj/promenade/4605.pdf
module TicketProblem where

import Data.Char (ord)

type Rat = (Int, Int)
data Term = Val Char | App Char Term Term

trees :: [Char] -> [Char] -> [Term]
trees ds os = [ t | (_, t) <- [ otree os u | u <- dtrees ds ]]

dtrees :: [Char] -> [Term]
dtrees [x] = [Val x]
dtrees ds = concat [ joins ls rs | (ls, rs) <- [ lrs xs ys | (xs, ys) <- splits1 ds ]]

splits1 :: [Char] -> [([Char], [Char])]
splits1 [x]    = []
splits1 (x:xs) = ([x], xs) : [ (x:ys, zs) | (ys, zs) <- splits1 xs ]

lrs :: [Char] -> [Char] -> ([Term], [Term])
lrs xs ys = (dtrees xs, dtrees ys)

joins :: [Term] -> [Term] -> [Term]
joins ls rs = [ App '^' l r | l <- ls, r <- rs ]

otree :: [Char] -> Term -> ([Char], Term)
otree os (Val c)     = (os, Val c)
otree os (App _ l r) = (os'', App o' l' r')
  where (o':os', l') = otree os  l
        (os''  , r') = otree os' r

instance Show Term where
  show (Val c) = [c]
  show (App o l r) = "(" ++ show l ++ [o] ++ show r ++ ")"

eval :: Term -> Rat
eval (Val x)     = ctor x
eval (App o l r) = (ctoo o) (eval l) (eval r)

ctor :: Char -> Rat
ctor x = (ord x - ord '0', 1)

ctoo :: Char -> (Rat -> Rat -> Rat)
ctoo '+' (x, y) (z, w) = (x*w+z*y, y*w)
ctoo '-' (x, y) (z, w) = (x*w-z*y, y*w)
ctoo '*' (x, y) (z, w) = (x*z, y*w)
ctoo '/' (x, y) (z, w) = if z == 0 then (0, 0) else (x*w, y*z)

ticket :: Int -> [Char] -> Term
ticket n ds = head (filter (same n) (allterms ds))

same :: Int -> (Term -> Bool)
same i t = i*d == n && d /= 0
  where (n, d) = eval t

allterms :: [Char] -> [Term]
allterms ds = concat [ trees ns os | ns <- perm ds, os <- rperm ops4 (length ds - 1) ]
  where pm _ [] = []
        pm hs (t:ts) = [ t:ys | ys <- perm (hs ++ ts) ]

ops4 = "+-*/"

perm [] = [[]]
perm xs = concat [ pm hs ts | (hs, ts) <- splits xs ]
  where pm _  []     = []
        pm hs (t:ts) = [ t:ys | ys <- perm (hs ++ ts) ]

splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : [ (x:ys, zs) | (ys, zs) <- splits xs ]

rperm _  0 = [[]]
rperm [] _ = []
rperm xs n = [ x:ys | x <- xs, ys <- rperm xs (n-1) ]
