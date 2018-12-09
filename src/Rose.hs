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

parat (g, c, h) (Node a fs) = g a (fs, (paraf (g, c, h) fs))
paraf (g, c, h) Nil = c
paraf (g, c, h) (Cons t fs) = h (t, parat (g, c, h) t) (fs, paraf (g, c, h) fs)

(<>) :: Forest a -> Forest a -> Forest a
xs <> ys = paraf (g, c, h) xs
  where
    g t (fs, _) = node (t, fs)
    c = ys
    h (_, t') (_, fs') = cons (t', fs')

etat :: a -> Tree a
etat = node . pair (id, etaf)
etaf :: a -> Forest a
etaf = const nil
-- etaf = grows . pair (etat, const null)

mut :: Tree (Tree a) -> Tree a
mut (Node (Node a fa) fta) = node (a, fa <> muft fta)
muft :: Forest (Tree a) -> Forest a
muft Nil = nil
muft (Cons tta fta) = cons (mut tta, muft fta)
