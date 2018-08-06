{-# LANGUAGE LambdaCase #-}
module MutualRecursiveTreeForest where

import Prelude hiding (null)

cross (f, g) (x, y) = (f x, g y)

data Tree a = Fork a (Forest a) deriving (Show, Eq)
data Forest a = Null
              | Grows (Tree a) (Forest a)
              deriving (Show, Eq)

fork = uncurry Fork
null = Null
grows = uncurry Grows

foldt (g, c, h) (Fork x fs) = g (x, foldf (g, c, h) fs)

foldf (g, c, h) Null = c
foldf (g, c, h) (Grows t fs) = h (foldt (g, c, h) t, foldf (g, c, h) fs)

unfoldt b@(phi, psi) t = case phi t of
  (a, f') -> Fork a (unfoldf b f')

unfoldf b@(phi, psi) f = case psi f of
  Nothing -> Null
  Just (t', f') -> Grows (unfoldt b t') (unfoldf b f')

-- trivials

idt = foldt (fork, null, grows)
idf = foldf (fork, null, grows)

(idt', idf') = (unfoldt (phi, psi), unfoldf (phi, psi))
  where
    phi (Fork a f) = (a, f)
    psi Null = Nothing
    psi (Grows t f) = Just (t, f)

(genT, genF) = (unfoldt (phi, psi), unfoldf (phi, psi))
  where
    phi n = (n, n-1)
    psi n = if n <= 0 then Nothing else Just (n, n-1)

-- utility
(lenT, lenF) = (foldt (g, c, h), foldf (g, c, h))
  where
    g (_, f) = 1 + f
    c = 0
    h (l, r) = l + r

(depthT, depthF) = (foldt (g, c, h), foldf (g, c, h))
  where
    g (_, f) = 1 + f
    c = 0
    h (l, r) = max l r

(sumT, sumF) = (foldt (g, c, h), foldf (g, c, h))
  where
    g (a, f) = a + f
    c = 0
    h (l, r) = l + r

-- type functor

mapt f (Fork a fs) = Fork (f a) (mapf f fs)
mapf f Null = Null
mapf f (Grows t fs) = Grows (mapt f t) (mapf f fs)

(mapt', mapf') = (genMap foldt, genMap foldf)
  where
    genMap cata f = cata (g, c, h)
      where
        g = fork . cross (f, id)
        c = null
        h = grows . cross (id, id)

parat (g, c, h) (Fork a fs) = g a (fs, (paraf (g, c, h) fs))
paraf (g, c, h) Null = c
paraf (g, c, h) (Grows t fs) = h (t, parat (g, c, h) t) (fs, paraf (g, c, h) fs)


fixT (ｔ, f) = \case
  Fork x xs -> Fork x (f xs)

fixF (t, f) = \case
  Null -> Null
  Grows xs ys -> Grows (t xs) (f ys)

(idT, idF) = (fixT (idT, idF), fixF (idT, idF))

instance Functor Tree where
  fmap = mapt
  x <$ (Fork _ fs) = Fork x (x <$ fs)

instance Functor Forest where
  fmap = mapf
  x <$ Null = Null
  x <$ (Grows t fs) = Grows (x <$ t) (x <$ fs)

instance Applicative Tree where
  pure = etat
  (<*>) = undefined

instance Applicative Forest where
  pure = etaf
  (<*>) = undefined

instance Monad Tree where
  return = etat
  m >>= f = mut (fmap f m)

instance Monad Forest where
  return = etaf
  m >>= f = muf (fmap f m)

(<>) :: Forest a -> Forest a -> Forest a
xs <> ys = paraf (g, c, h) xs
  where
    g t (fs, _) = fork (t, fs)
    c = ys
    h (_, t') (_, fs') = grows (t', fs')

etat x = fork (x,null)
etaf x = grows (etat x, null)

mut :: Tree (Tree a) -> Tree a
mut = undefined

muf :: Forest (Forest a) -> Forest a
muf = undefined
