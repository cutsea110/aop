module Defunctionalization where

-- kazu_yamamoto's question at haskell-jp slack.2019-04-05
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

foldt (f, g) = u
  where
    u (Leaf a) = f a
    u (Node l r) = g (u l) (u r)

cons x xs = x : xs
o f g x = f (g x)
flatten t = walk t []
walk = foldt (cons, o)

----------------------

data Lam a = LamCons a | LamO (Lam a) (Lam a) deriving Show

foldlam (f, g) = u
  where
    u (LamCons a) = f a
    u (LamO l r) = g (u l) (u r)

apply = foldlam (cons, o)

cons_def = LamCons
o_def = LamO
flatten_def t = apply (walk_def t) []
walk_def = foldt (cons_def, o_def)
