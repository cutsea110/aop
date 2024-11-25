{-# LANGUAGE LambdaCase #-}
module MutualRecursiveTreeForest where

import Prelude hiding (null,(<>))

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

data Tree a = Fork a (Forest a) deriving (Show, Eq)
data Forest a = Null | Grows (Tree a) (Forest a) deriving (Show, Eq)

fork = uncurry Fork
null = Null
grows = uncurry Grows

{-
-- NOTE: これは書けないので...
(foldt (g, c, h), foldf (g, c, h)) = (u, v)
  where
    u (Fork x fs)  = g (x, v fs)
    v Null         = c
    v (Grows t fs) = h (u t, v fs)

-- こう書くのが自然ではあるが図式的には上の方が対応がわかりやすい
foldt (g, c, h) (Fork x fs) = g (x, foldf (g, c, h) fs)

foldf (g, c, h) Null = c
foldf (g, c, h) (Grows t fs) = h (foldt (g, c, h) t, foldf (g, c, h) fs)
-}

-- 妥協案
(foldt, foldf) = (u, v)
  where
    u phi@(g, c, h) = \case
      (Fork x fs) -> g (x, v phi fs)
    v phi@(g, c, h) = \case
      Null -> c
      (Grows t fs) -> h (u phi t, v phi fs)

{-
-- NOTE: これもこうは書けないので...
(unfoldt (phi, psi), unfoldf (phi, psi)) = (u, v)
  where
    u x = case phi x of
      (a, f') -> Fork a (v f')
    v f = case psi f of
      Nothing -> Null
      Just (t', f') -> Grows (u t') (v f')

-- こう書くのが自然ではあるが図式的には上の方が対応がわかりやすい
unfoldt b@(phi, psi) t = case phi t of
  (a, f') -> Fork a (unfoldf b f')

unfoldf b@(phi, psi) f = case psi f of
  Nothing -> Null
  Just (t', f') -> Grows (unfoldt b t') (unfoldf b f')
-}

-- 妥協案
(unfoldt, unfoldf) = (u, v)
  where
    u psi@(f, g) x = case f x of
      (a, fs) -> Fork a (v psi fs)
    v psi@(f, g) x = case g x of
      Nothing -> Null
      Just (t, fs) -> Grows (u psi t) (v psi fs)


-- trivials
{-
-- 直接的ではあるが積圏で同時に定義されている感がない
idt = foldt (fork, null, grows)
idf = foldf (fork, null, grows)
-}

(idt, idf) = (k unfoldt, k unfoldf)
  where
    k ana = ana (f, g)
    f (Fork a fs) = (a, fs)
    g Null = Nothing
    g (Grows t fs) = Just (t, fs)

(genT, genF) = (k unfoldt, k unfoldf)
  where
    k ana = ana (f, g)
    f n = (n, n-1)
    g n = if n <= 0 then Nothing else Just (n, n-1)

-- utility
(lenT, lenF) = (k foldt, k foldf)
  where
    k cata = cata (g, c, h)
    g (_, fs) = 1 + fs
    c = 0
    h (t, fs) = t + fs

(depthT, depthF) = (k foldt, k foldf)
  where
    k cata = cata (g, c, h)
    g (_, fs) = 1 + fs
    c = 0
    h (t, fs) = max t fs

(sumT, sumF) = (k foldt, k foldf)
  where
    k cata = cata (g, c, h)
    g (a, fs) = a + fs
    c = 0
    h (t, fs) = t + fs

(prodT, prodF) = (k foldt, k foldf)
  where
    k cata = cata (g, c, h)
    g (a, fs) = a * fs
    c = 1
    h (t, fs) = t * fs

(flattenT, flattenF) = (k foldt, k foldf)
  where
    k cata = cata (g, c, h)
    g (a, fs) = a:fs
    c = []
    h (t, fs) = t ++ fs


-- on exponential
(zipT, zipF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
    h (a, fs) = \(Fork a' fs') -> Fork (a, a') (fs fs')
    c = \_ -> Null
    g (t, fs) = \(Grows t' fs') -> Grows (t t') (fs fs')

unzipT = pair (mapt fst, mapt snd)
unzipF = pair (mapf fst, mapf snd)

-- type functor
{-
-- 直接的ではあるが積圏で同時に定義されている感がない
mapt f (Fork a fs) = Fork (f a) (mapf f fs)
mapf f Null = Null
mapf f (Grows t fs) = Grows (mapt f t) (mapf f fs)
-}
-- 現状わりとうまく f を導入できている案
(mapt, mapf) = (k foldt, k foldf)
  where
    k cata f = cata (g, c, h)
      where
        g = fork . cross (f, id)
        c = null
        h = grows . cross (id, id)

(parat, paraf) = (u, v)
  where
    u phi@(g, c, h) = \case
      (Fork a fs) -> g a (fs, v phi fs)
    v phi@(g, c, h) = \case
      Null -> c
      (Grows t fs) -> h (t, u phi t) (fs, v phi fs)

{-    
parat (g, c, h) (Fork a fs) = g a (fs, paraf (g, c, h) fs)
paraf (g, c, h) Null = c
paraf (g, c, h) (Grows t fs) = h (t, parat (g, c, h) t) (fs, paraf (g, c, h) fs)
-}

fixT (f, g) = \case
  Fork a fs -> Fork a (g fs)

fixF (f, g) = \case
  Null -> Null
  Grows t fs -> Grows (f t) (g fs)

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
  Fork f fs <*> t@(Fork x xs) =
    let Fork x' xs' = fmap f t
    in Fork x' (xs' <> fmap ($ x) fs <> (fs <*> xs))

instance Applicative Forest where
  pure = etaf
  Grows ft ff <*> Grows xt xf = grows (ft <*> xt, ff <*> xf)
  _ <*> _ = null

instance Monad Tree where
  return = pure
  m >>= f = mut (fmap f m)

instance Monad Forest where
  return = pure
  m >>= f = muf (fmap f m)

(<>) :: Forest a -> Forest a -> Forest a
xs <> ys = paraf (g, c, h) xs
  where
    g t (fs, _) = fork (t, fs)
    c = ys
    h (_, t') (_, fs') = grows (t', fs')

etat :: a -> Tree a
etat = fork . pair (id, etaf)
etaf :: a -> Forest a
etaf = const null
-- etaf = grows . pair (etat, const null)

mut :: Tree (Tree a) -> Tree a
mut (Fork (Fork a fa) fta) = fork (a, fa <> muft fta)
muft :: Forest (Tree a) -> Forest a
muft Null = null
muft (Grows tta fta) = grows (mut tta, muft fta)

mutf :: Tree (Forest a) -> Tree a
mutf (Fork Null xs) = fork (undefined, muf xs)
mutf (Fork (Grows (Fork t xs) ys) zs) = fork (t, xs <> ys <> muf zs)
muf :: Forest (Forest a) -> Forest a
muf Null = null
muf (Grows t fs) = grows (mutf t, muf fs)
