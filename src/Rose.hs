module Rose where

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

data Tree a = Node a (Forest a) deriving Show
data Forest a = Nil | Cons (Tree a) (Forest a) deriving Show

node = uncurry Node
nil = Nil
cons = uncurry Cons

foldt (g, c, h) (Node a f) = g (a, foldf (g, c, h) f)
foldf (g, c, h) Nil        = c
foldf (g, c, h) (Cons t f) = h (foldt (g, c, h) t, foldf (g, c, h) f)

mapt f (Node a fs) = node (f a, mapf f fs)
mapf f Nil = nil
mapf f (Cons t fs) = cons (mapt f t, mapf f fs)

(mapt', mapf') = (genMap foldt, genMap foldf)
  where
    genMap cata f = cata (g, c, h)
      where
        g = node . cross (f, id)
        c = nil
        h = cons . cross (id, id)
