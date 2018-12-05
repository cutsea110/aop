module Traditional where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

empty = Empty
node (a, l, r) = Node a l r

foldtt (c, h) Empty = c
foldtt (c, h) (Node x l r) = h (x, foldtt (c, h) l, foldtt (c, h) r)

maptt f = foldtt (empty, node . cross3 (f, id, id))
    where
        cross3 (f, g, h) (x, y, z) = (f x, g y, h z)

eta = const empty
mu :: Tree (Tree a) -> Tree a
mu Empty = empty
mu (Node x l r) = empty

instance Functor Tree where
    fmap = maptt

instance Applicative Tree where
    pure = eta
    Empty <*> x = empty
    Node f l r <*> x = node (undefined, l <*> x, r <*> x)
