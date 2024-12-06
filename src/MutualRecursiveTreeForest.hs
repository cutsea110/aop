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
(foldt (h, c, g), foldf (h, c, g)) = (u, v)
  where
    u (Fork a fs)  = h (a, v fs)
    v Null         = c
    v (Grows t fs) = g (u t, v fs)

-- こう書くのが自然ではあるが図式的には上の方が対応がわかりやすい
foldt phi@(h, c, g) (Fork a fs) = g (a, foldf phi fs)

foldf phi@(h, c, g) Null = c
foldf phi@(h, c, g) (Grows t fs) = h (foldt phi t, foldf phi fs)
-}

-- 妥協案
(foldt, foldf) = (u, v)
  where
    u phi@(h, c, g) = \case
      (Fork a fs) -> h (a, v phi fs)
    v phi@(h, c, g) = \case
      Null -> c
      (Grows t fs) -> g (u phi t, v phi fs)

{-
-- NOTE: これもこうは書けないので...
(unfoldt (f, g), unfoldf (f, g)) = (u, v)
  where
    u x = case f x of
      (a, fs) -> Fork a (v fs)
    v x = case g x of
      Nothing -> Null
      Just (t, fs) -> Grows (u t) (v fs)

-- こう書くのが自然ではあるが図式的には上の方が対応がわかりやすい
unfoldt psi@(f, g) x = case f x of
  (a, fs) -> Fork a (unfoldf psi fs)

unfoldf psi@(f, g) x = case g x of
  Nothing -> Null
  Just (t, fs) -> Grows (unfoldt psi t) (unfoldf psi fs)
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
      where
        f (Fork a fs) = (a, fs)
        g Null = Nothing
        g (Grows t fs) = Just (t, fs)

(genT, genF) = (k unfoldt, k unfoldf)
  where
    k ana = ana (f, g)
      where
        f n = (n, n-1)
        g n = if n <= 0 then Nothing else Just (n, n-1)

(genT', genF') = (k unfoldt, k unfoldf)
  where
    k ana = ana (f, g)
      where
        f n = (n, n+1)
        g n = Just (n, n+1)

-- utility
(lenT, lenF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
      where
        h (_, fs) = 1 + fs
        c = 0
        g (t, fs) = t + fs

(depthT, depthF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
      where
        h (_, fs) = 1 + fs
        c = 0
        g (t, fs) = max t fs

(sumT, sumF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
      where
        h (a, fs) = a + fs
        c = 0
        g (t, fs) = t + fs

(prodT, prodF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
      where
        h (a, fs) = a * fs
        c = 1
        g (t, fs) = t * fs

(flattenT, flattenF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
      where
        h (a, fs) = [a]:fs
        c = []
        g (t, fs) = t ++ fs


takeWhileT p = foldt (h, c, g)
  where
    h (a, fs) | p a       = Fork a fs
              | otherwise = Fork a Null
    c = Null
    g (t@(Fork a _), fs) | p a       = Grows t fs
                         | otherwise = Null

takeWhileF p = foldf (h, c, g)
  where
    h (a, fs) | p a       = Fork a fs
              | otherwise = Fork a Null
    c = Null
    g (t@(Fork a _), fs) | p a       = Grows t fs
                         | otherwise = Null

drawT :: Show a => Tree a -> String
drawT = concat . foldt (h, c, g)
  where
    h :: Show a => (a, [String]) -> [String]
    h (a, fs) = ("+-- " ++ show a ++ "\n") : map ("  " `joint`) fs
      where cs `joint` ds@('+':_) = cs ++ ds
            cs `joint` (' ':ds) = cs ++ '|':ds
    c :: [String]
    c = ["+- *\n"]
    g :: ([String], [String]) -> [String]
    g (t, fs) = t ++ fs

drawF :: Show a => Forest a -> String
drawF = concat . foldf (h, c, g)
  where
    h :: Show a => (a, [String]) -> [String]
    h (a, fs) = ("+-- " ++ show a ++ "\n") : map ("  " `joint`) fs
      where cs `joint` ds@('+':_) = cs ++ ds
            cs `joint` (' ':ds) = cs ++ '|':ds
    c :: [String]
    c = ["+- *\n"]
    g :: ([String], [String]) -> [String]
    g (t, fs) = t ++ fs

-- on exponential
(zipT, zipF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
      where
        h (a, fs) = \(Fork a' fs') -> Fork (a, a') (fs fs')
        c = \_ -> Null
        g (t, fs) = \(Grows t' fs') -> Grows (t t') (fs fs')



(unzipT, unzipF) = (pair (mapt fst, mapt snd), pair (mapf fst, mapf snd))

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
    k cata f = cata (h, c, g)
      where
        h = fork . cross (f, id)
        c = null
        g = grows . cross (id, id)

(parat, paraf) = (u, v)
  where
    u phi@(h, c, g) = \case
      (Fork a fs) -> h a (fs, v phi fs)
    v phi@(h, c, g) = \case
      Null -> c
      (Grows t fs) -> g (t, u phi t) (fs, v phi fs)

{-    
parat phi@(h, c, g) (Fork a fs) = h a (fs, paraf phi fs)
paraf phi@(h, c, g) Null = c
paraf phi@(h, c, g) (Grows t fs) = g (t, parat phi t) (fs, paraf phi fs)
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

infixl 9 <>
(<>) :: Forest a -> Forest a -> Forest a
xs <> ys = paraf (h, c, g) xs
  where
    h t (fs, _) = fork (t, fs)
    c = ys
    g (_, t') (_, fs') = grows (t', fs')

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
