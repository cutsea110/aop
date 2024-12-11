{-# LANGUAGE LambdaCase #-}
module MutualRecursiveTreeForest where

import Prelude hiding (null,(<>))
import Data.Maybe (fromJust)

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
    h (a, fs) = ("+-- " ++ show a ++ "\n") : map ("  " `joint`) fs
      where cs `joint` ds@('+':_) = cs ++ ds
            cs `joint` ds         = cs ++ '|':ds
    c = []
    g (t, fs) = t ++ fs

drawF :: Show a => Forest a -> String
drawF = concat . foldf (h, c, g)
  where
    h (a, fs) = ("+-- " ++ show a ++ "\n") : map ("  " `joint`) fs
      where cs `joint` ds@('+':_) = cs ++ ds
            cs `joint` ds         = cs ++ '|':ds
    c = []
    g (t, fs) = t ++ fs


showT :: Show a => Tree a -> IO ()
showT = putStr . drawT

showF :: Show a => Forest a -> IO ()
showF = putStr . drawF


-- on exponential
-- zip の場合は二つの引数でサイズが違うときは小さい方に詰められる
-- 個人的には自由定理によって選択の余地がないことから一意に決まると考えたい派閥ではあり、
-- その観点だとサイズが同じと制約を入れる方が楽ではある(選択の余地が出てくると意味を考えて実装する必要がある)
-- 一方、使いやすさの観点だとサイズ違いの場合をドメインとして受け付けたい
-- zip に関しては仮に選択の余地があるとしても短い方につぶすしかないので対応してもさほど問題はないので許容範囲ではある
-- この実装はサイズが異なる場合にも対応しているが、選択の余地はほぼなくて自由定理から一意に定まる範囲ではないかとは思う
(zipT, zipF) = (k foldt, k foldf)
  where
    k cata = cata (h, c, g)
      where
        h (a, fs) = \case
          (Fork b fb) -> Fork (a, b) (fs fb)
        c = \case
          Null -> Null
          Grows _ _ -> Null
        g (t, fs) = \case
          (Grows tb fb) -> Grows (t tb) (fs fb)
          Null -> Null

(unzipT, unzipF) = (pair (mapt fst, mapt snd), pair (mapf fst, mapf snd))



data Tuple a b = Fst a | Snd b | Tuple a b deriving Show
tuple f s t = \case
  Fst a     -> f a
  Snd b     -> s b
  Tuple a b -> t a b

-- zipt, zipf のシンプルな実装
zipt (Fork a afs) (Fork b bfs) = Fork (Tuple a b) (zipf afs bfs)

zipf (Grows at afs) (Grows bt bfs) = Grows (zipt at bt) (zipf afs bfs)
zipf (Grows at afs) Null = Grows (mapt Fst at) (zipf afs Null)
zipf Null (Grows bt bfs) = Grows (mapt Snd bt) (zipf Null bfs)
zipf Null Null = Null

-- | cata では残念ながら実装できない
{--
(zipt', zipf') = (foldt (h, c, g), foldf (h, c, g))
  where
    h :: (a, Forest b -> Forest (Tuple a b)) -> Tree b -> Tree (Tuple a b)
    h (a, fs) = \(Fork b bfs) -> Fork (Tuple a b) (fs bfs)
    c :: Forest b -> Forest (Tuple a b)
    c = \case
      Null -> Null
      Grows bt bfs -> Grows (mapt Snd bt) (mapf Snd bfs)
    g :: (Tree b -> Tree (Tuple a b), Forest b -> Forest (Tuple a b)) -> Forest b -> Forest (Tuple a b)
    g (t, fs) = \case
      Null -> Null -- ここが成立しない
      Grows bt bfs -> Grows (t bt) (fs bfs)
--}

-- para を使えば実装可能
(zipt', zipf') = (parat (h, c, g), paraf (h, c, g))
  where
    h :: (a, (Forest a, Forest b -> Forest (Tuple a b))) -> (Tree b -> Tree (Tuple a b))
    h (a, (_, fs)) = \case
      Fork b bfs -> Fork (Tuple a b) (fs bfs)
    c :: Forest b -> Forest (Tuple a b)
    c = \case
      Null -> Null
      Grows bt bfs -> Grows (mapt Snd bt) (mapf Snd bfs)
    g :: ((Tree a, Tree b -> Tree (Tuple a b)), (Forest a, Forest b -> Forest (Tuple a b))) -> Forest b -> Forest (Tuple a b)
    g ((ta, t), (fa, fs)) = \case
      Null -> Grows (mapt Fst ta) (mapf Fst fa)
      Grows bt bfs -> Grows (t bt) (fs bfs)

pi1 :: Tuple a b -> Maybe a
pi1 (Fst a) = Just a
pi1 (Tuple a _) = Just a
pi1 _ = Nothing
pi2 :: Tuple a b -> Maybe b
pi2 (Fst _) = Nothing
pi2 (Tuple _ b) = Just b
pi2 (Snd b) = Just b

unzipt' :: Tree (Tuple a b) -> (Tree a, Tree b)
unzipt' t = (fromJust $ upt $ mapt pi1 t, fromJust $ upt $ mapt pi2 t)
unzipf' :: Forest (Tuple a b) -> (Forest a, Forest b)
unzipf' f = (upf $ mapf pi1 f, upf $ mapf pi2 f)

upt :: Tree (Maybe a) -> Maybe (Tree a)
upt (Fork Nothing _) = Nothing
upt (Fork (Just a) fs) = Just (Fork a (upf fs))
upf :: Forest (Maybe a) -> Forest a
upf Null = Null
upf (Grows t fs) = case upt t of
  Nothing -> upf fs
  Just t' -> Grows t' (upf fs)

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

parat' :: ((a, (Forest a, y)) -> x, y, ((Tree a, x), (Forest a, y)) -> y) -> Tree a -> x
parat' phi@(h, c, g) (Fork a fs) = h (a, (fs, paraf' phi fs))
paraf' :: ((a, (Forest a, y)) -> x, y, ((Tree a, x), (Forest a, y)) -> y) -> Forest a -> y
paraf' phi@(h, c, g) Null = c
paraf' phi@(h, c, g) (Grows t fs) = g ((t, parat' phi t), (fs, paraf' phi fs))

parat :: ((a, (Forest a, y)) -> x, y, ((Tree a, x), (Forest a, y)) -> y) -> Tree a -> x
paraf :: ((a, (Forest a, y)) -> x, y, ((Tree a, x), (Forest a, y)) -> y) -> Forest a -> y
(parat, paraf) = (u, v)
  where
    u phi@(h, c, g) = \case
      (Fork a fs) -> h (a, (fs, v phi fs))
    v phi@(h, c, g) = \case
      Null -> c
      (Grows t fs) -> g ((t, u phi t), (fs, v phi fs))

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
    h (t, (fs, _)) = fork (t, fs)
    c = ys
    g ((_, t'), (_, fs')) = grows (t', fs')

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
