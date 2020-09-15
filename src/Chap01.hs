{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chap01 where

import Prelude hiding (last, foldr, foldl, take, drop, zip, concat, inits, reverse)

-- | Ex 1.1
-- solution 1
k1 x = if k1 x == 0 then 1 else 0

-- solution 2
k2 x = not (k2 x)

-- solution 3
k3 x = k3 x + 1

-- solution 4
k4 x = k4 x ++ [x]

-- | Ex 1.2
m (x, y) = if x == y
           then y + 1
           else m (x, m (x - 1, y + 1))
-- solution 1
m1 (x, y) = x + 1

-- solution 2
m2 (x, y) = if x >= y
            then x + 1
            else y - 1

-- | Ex 1.3

data Nat = Zero
         | Succ Nat
         deriving (Show, Eq)

foldn (c, f) = u
  where u Zero     = c
        u (Succ n) = f (u n)

data NatPlus = One
             | Next NatPlus
         deriving (Show, Eq)

foldnplus (c, f) = u
  where u One      = c
        u (Next n) = f (u n)

f :: NatPlus -> Nat
f = foldnplus (Zero, Succ)

g :: Nat -> NatPlus
g = foldn (One, Next)

-- test
testfgIsId n = n == (f . g) n
testgfIsId n = n == (g . f) n

-- | Ex 1.4
plus = foldn (id, (Succ .))
outl (x, _) = x
sqr = f . foldn (c, h)
  where f = outl
        c = (zero, zero)
        h (s, dn) = (plus (plus s dn) one, plus dn two)
        (zero, one, two) = (Zero, Succ zero, Succ one)

-- | Ex 1.5
last :: (Nat  -> Bool) -> Nat -> Nat
last p = f . foldn (c, h)
  where f = outl
        c = (Zero, Succ Zero)
        h (l, n) = if p n then (n, Succ n) else (l, Succ n)

test_1_5 = map (fromNat . last evenp . toNat) [0..100]
  where
    evenp, oddp :: Nat -> Bool
    evenp Zero = True
    evenp (Succ n) = oddp n
    oddp Zero = False
    oddp (Succ n) = evenp n

    toNat 0 = Zero
    toNat n = Succ (toNat (n-1))
    fromNat Zero = 0
    fromNat (Succ n) = 1 + fromNat n

-- | Ex 1.6
foldn' :: (a, a -> a) -> Int -> a
foldn' (c, f) = u
  where u 0 = c
        u (n+1) = f (u n)

{--
cack :: Nat -> Nat -> Nat
cack = foldn (Succ, swap f)
  where f = foldn (ap1, ap2)
        ap1 g = g (Succ Zero)
        ap2 g h = h (g h)
        swap f a b = f b a
--}

{--
-- step 0
ack (0, y) = y + 1
ack (x+1, 0) = ack (x, 1)
ack (x+1, y+1) = ack (x, ack (x+1, y))
--}

{--
-- step 1
cack 0 y = y + 1
cack (x+1) 0 = cack x 1
cack (x+1) (y+1) = cack x (cack (x+1) y)
--}

{--
-- step 2
f y = y + 1       -- f = cack 0
g 0 = h 1         -- g = cack (x+1), h = cack x
g (y+1) = h (g y) -- same as above
--}

{--
-- step 3
f = (+1)
g = foldn' (h 1, h)
--}


{--
-- step 4
cack 0 = (+1)
cack (x+1) = foldn' (cack x 1, cack x)
--}

{--
-- step 5
cack 0 = (+1)
cack (x+1) = k (cack x)
  where k v = foldn' (v 1, v)
--}

-- step 6
cack = foldn' ((+1), k)
  where k v = foldn' (v 1, v)

cack' = foldn' ((+1), swap f)
  where f = foldn' (ap1, ap2)
        ap1 g = g 1
        ap2 g h = h (g h)
        swap f a b = f b a

-- | Ex 1.7
data ListR a = Nil
             | Cons (a, ListR a)
             deriving Show

foldr :: (b, (a, b) -> b) -> ListR a -> b
foldr (c, f) = u
  where u Nil = c
        u (Cons (a, xs)) = f (a, u xs)

--         [nil, cons]
-- [a] <------------------------- 1 + a * [a]
--  |                               |
--  | u = (|nil, cons . (f * id)|)  | 1 + 1 * u
--  |                               |
--  v                               v
-- [b]  <----- 1 + b * [b] ------ 1 + a * [b]
--       [nil, cons]       1 + f * id
--      <------------------------
--        [nil, cons] . (1 + f * id) == [nil, cons . (f * id)
--
listr :: (a -> b) -> ListR a -> ListR b
listr f = foldr (Nil, Cons . cross (f, id))

data ListL a = SNil
             | Snoc (ListL a, a)
             deriving Show

foldl :: (b, (b, a) -> b) -> ListL a -> b
foldl (c, f) = u
  where u SNil = c
        u (Snoc (xs, a)) = f (u xs, a)

--         [snil, snoc]
-- [a] <------------------------- 1 + [a] * a
--  |                               |
--  | u = (|snil, snoc . (id * f)|) | 1 + u * 1
--  |                               |
--  v                               v
-- [b]  <----- 1 + [b] * b ------ 1 + [b] * a
--       [snil, snoc]     1 + id * f
--      <------------------------
--       [snil, snoc] . (1 + id * f) == [snil, snoc . (id * f)
listl :: (a -> b) -> ListL a -> ListL b
listl f = foldl (SNil, g)
  where g (xs, x) = Snoc (xs, f x)

{--
convert :: ListL a -> ListR a
convert SNil = Nil
convert (Snoc (xs, a)) = snocr (convert xs, a)
--}

convert :: ListL a -> ListR a
convert = foldl (Nil, snocr)

{--
snocr :: (ListR a, a) -> ListR a
snocr (Nil, b) = Cons (b, Nil)
snocr (Cons (a, x), b) = Cons (a, snocr (x, b))
--}

snocr :: (ListR a, a) -> ListR a
snocr = uncurry . flip $ snocr'
  where
    snocr' :: a -> ListR a -> ListR a
    snocr' b = foldr (c, f)
      where c = Cons (b, Nil)
            f = Cons
{--
snocr' b Nil = Cons (b, Nil)
snocr' b (Cons (a, x)) = Cons (a, snocr' b x)
--}

-- | Ex 1.8
{--
-- step 1
catconv SNil = id
catconv (Snoc (xs, a)) = \ys -> catconv xs (Cons (a, ys))
--}
{--
-- step 2
-- h (catconv xs, a) === \ys -> catconv xs (Cons (a, ys))
-- h (catconv xs, a) ys === catconv xs (Cons (a, ys))
-- h (f, a) ys === f (Cons (a, ys)) where f = catconv xs
catconv SNil = id
catconv (Snoc (xs, a)) = h (catconv xs, a)
  where h (f, a) ys = f (Cons (a, ys))
--}

-- step 3
catconv :: ListL a -> ListR a -> ListR a
catconv = foldl (c, h)
  where c = id
        h (f, a) ys = f (Cons (a, ys))

convert' :: ListL a -> ListR a
convert' x = catconv x Nil

-- | Ex 1.9
{--
nil ++ ys == ys

i) ys = nil の場合:
nil ++ nil = nil

ii) ys = Snoc (ys', y') の場合:
nil ++ Snoc (ys', y')
= {- ++ の定義 -}
Snoc (nil ++ ys', y')
= {- 帰納法 -}
Snoc (ys', y')
--}

-- | Ex 1.10

cat :: ListR a -> (ListR a -> ListR a)
cat Nil = id
cat (Cons (x, xs)) = \ys -> Cons (x, cat xs ys)

-- | Ex 1.11

-- foldl over cons-list
{--
-- step 1
foldL :: (b, (b, a) -> b) -> ListR a -> b
foldL (c, f) Nil = c
foldL (c, f) (Cons (x, xs)) = foldL (f (c, x), f) xs
--}

-- [a] <----------------------- 1 + a * [a]
--  |                             |
--  |u = (|id, g|)                | 1 + 1 * (|id, g|)
--  |                             |
--  v                             v
-- b^b <----------------------- 1 + a * b^b
--
foldL :: forall a b. (b, (b, a) -> b) -> ListR a -> b
foldL (c, f) x = foldr (id, g) x c
  where
    g :: (a, b -> b) -> b -> b
    g (a, h) c = h (f (c, a))

-- | Ex 1.12
take :: Nat -> ListR a -> ListR a
take n x = foldr (c, h) x n
  where
    c :: Nat -> ListR a
    c a = Nil
    h :: (a, Nat -> ListR a) -> Nat -> ListR a
    h (a, f) Zero = Nil
    h (a, f) (Succ n) = Cons (a, f n)

drop :: Nat -> ListR a -> ListR a
drop n x = foldr (d, k) x n
  where
    d :: Nat -> ListR a
    d a = Nil
    k :: (a, Nat -> ListR a) -> Nat -> ListR a
    k (a, f) Zero = Cons (a, f Zero)
    k (a, f) (Succ n) = f n

-- | Ex 1.13
data GTree a = Node (a, ListL (GTree a)) deriving Show

--               Node
-- GTree A <----------------- A * [Gtree A]
--     |                        |
--     | (|f|)                  | F(|f|)
--     |                        |
--     v                        v
--     B   <----------------- A * [B]
--                 f
foldg :: ((a, ListL b) -> b) -> GTree a -> b
foldg f = u
  where u (Node (x, ts)) = f (x, listl u ts)

size :: GTree a -> Integer
size = foldg g
  where g (x, ts) = 1 + sum ts
        sum :: ListL Integer -> Integer
        sum = foldl (c, f)
          where c = 0
                f = uncurry (+)

depth :: GTree a -> Integer
depth = foldg g
  where g (a, SNil) = 0
        g (a, Snoc (xs, x)) = 1 + maxlist xs
        maxlist :: ListL Integer -> Integer
        maxlist = foldl (c, f)
          where c = 0
                f = uncurry max

-- | Ex 1.14

data Tree a = Tip a
            | Bin (Tree a, Tree a)
            deriving Show

foldt :: (a -> b, (b, b) -> b) -> Tree a -> b
foldt (f, g) = u
  where u (Tip a) = f a
        u (Bin (l, r)) = g (u l, u r)

tree :: (a -> b) -> Tree a -> Tree b
tree f = foldt (Tip . f, Bin)

curryT :: GTree a -> Tree a
curryT = foldg h
  where h (x, ts) = foldl (Tip x, Bin) ts

uncurryT :: Tree a -> GTree a
uncurryT = foldt (f, g)
  where f a = Node (a, SNil)
        g (Node (a, xs), ys) = Node (a, Snoc (xs, ys))

-- f (g (a, b), h (c), d)
test_1_14 = Node ('f', ghd)
  where a = Node ('a', SNil)
        b = Node ('b', SNil)
        c = Node ('c', SNil)
        d = Node ('d', SNil)
        ab = Snoc (Snoc (SNil, a), b)
        gab = Node ('g', ab)
        hc = Node ('h', Snoc (SNil, c))
        ghd = Snoc (Snoc (Snoc (SNil, gab), hc), d)

-- | Ex 1.15
-- [a] <----------------------- 1 + a * [a]
--  |                             |
--  |u = (|id, g|)                | 1 + 1 * (|id, g|)
--  |                             |
--  v                             v
-- [(a,b)]^[b] <--------------- 1 + a * [(a,b)]^[b]
--
zip :: ListR a -> ListR b -> ListR (a, b)
zip = foldr (c, h)
  where
    c :: ListR b -> ListR (a, b)
    c ys = Nil
    h :: (a, ListR b -> ListR (a, b)) -> ListR b -> ListR (a, b)
    h (x, f) Nil = Nil
    h (x, f) (Cons (y, ys)) = Cons ((x, y), f ys)

-- | Ex 1.16
data D = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving Show
data D' = D'1 | D'2 | D'3 | D'4 | D'5 | D'6 | D'7 | D'8 | D'9 deriving Show

data Digits = Wrap D' | Add (Digits, D) deriving Show

foldd :: (D' -> a, (a, D) -> a) -> Digits -> a
foldd (f, g) = u
  where u (Wrap d) = f d
        u (Add (ds, d)) = g (u ds, d)

plus' :: NatPlus -> NatPlus -> NatPlus
plus' = foldnplus (c, f)
  where c y = Next y
        f g y = Next (g y)
-- plus' One y = Next y
-- plus' (Next x) y = Next (plus' x y)
times' :: NatPlus -> NatPlus -> NatPlus
times' = foldnplus (c, f)
  where c y = y
        f g y = plus' y (g y)
-- times' One y = y
-- times' (Next x) y = plus' y (times' x y)

unfoldd :: (a -> Either D' (a, D)) -> a -> Digits
unfoldd psi = v
  where v x = case psi x of
          Left  d'      -> Wrap d'
          Right (ds, d) -> Add (v ds, d)


toD' :: Integer -> D'
toD' 1 = D'1
toD' 2 = D'2
toD' 3 = D'3
toD' 4 = D'4
toD' 5 = D'5
toD' 6 = D'6
toD' 7 = D'7
toD' 8 = D'8
toD' 9 = D'9
toD' _ = error "Oops!"

fromD' :: D' -> NatPlus
fromD' D'1 = One
fromD' D'2 = Next (fromD' D'1)
fromD' D'3 = Next (fromD' D'2)
fromD' D'4 = Next (fromD' D'3)
fromD' D'5 = Next (fromD' D'4)
fromD' D'6 = Next (fromD' D'5)
fromD' D'7 = Next (fromD' D'6)
fromD' D'8 = Next (fromD' D'7)
fromD' D'9 = Next (fromD' D'8)

toD :: Integer -> D
toD 0 = D0
toD 1 = D1
toD 2 = D2
toD 3 = D3
toD 4 = D4
toD 5 = D5
toD 6 = D6
toD 7 = D7
toD 8 = D8
toD 9 = D9
toD _ = error "Oops!"

fromD :: D -> NatPlus
fromD D0 = error "Oops!"
fromD D1 = One
fromD D2 = Next (fromD D1)
fromD D3 = Next (fromD D2)
fromD D4 = Next (fromD D3)
fromD D5 = Next (fromD D4)
fromD D6 = Next (fromD D5)
fromD D7 = Next (fromD D6)
fromD D8 = Next (fromD D7)
fromD D9 = Next (fromD D8)

decimal :: NatPlus -> Digits
decimal = unfoldd psi
  where psi :: NatPlus -> Either D' (NatPlus, D)
        psi n = case fromNatPlus n `divMod` 10 of
          (0, m) -> Left (toD' m)
          (d, m) -> Right (toNatPlus d, toD m)

eval :: Digits -> NatPlus
eval = foldd (fromD', p)
  where d10 = Next (fromD D9)
        p :: (NatPlus, D) -> NatPlus
        p (n, D0) = n `times'` d10
        p (n, d)  = (n `times'` d10) `plus'` fromD d

fromNatPlus :: NatPlus -> Integer
fromNatPlus = foldnplus (1, (1+))
toNatPlus :: Integer -> NatPlus
toNatPlus n | n == 1 = One
            | n > 1  = Next (toNatPlus (n-1))
            | otherwise = error "Oops!"

test_1_16 :: NatPlus
test_1_16 = eval (Add (Wrap D'4, D2))
test_1_16' :: Integer -> Digits
test_1_16' = decimal . toNatPlus

-- | Ex 1.17
--
-- 1. listr f . concat == concat . listr (listr f)
--       where concat = foldr (nil, cat)
--             cat x = foldl (x, snoc)
--
concat :: ListR (ListR a) -> ListR a
concat = foldr (Nil, uncurry cat)

--  Lemma. map f (xs ++ ys) == map f xs ++ map f ys を証明する(ref IFPH exercise 4.3.4)
--
--  base case: xs == [] {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  map f ([] ++ ys)
-- == {- cat の定義 -}
--  map f ys
--
--  base case: xs == [] {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  map f xs ++ map f ys
-- == {- map(foldr) の定義 -}
--  [] ++ map f ys
-- == {- cat の定義 -}
--  map f ys
--
-- inductive case: xs == (x:xs) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  map f ((x:xs) ++ ys)
-- == {- cat の定義 -}
--  map f (x:(xs ++ ys))
-- == {- map(foldr) の定義 -}
--  f x:map f (xs ++ ys)
-- == {- 帰納法 -}
--  f x:(map f xs ++ map f ys)
--
-- inductive case: xs == (x:xs) {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  map f (x:xs) ++ map f ys
-- == {- map(foldr) の定義 -}
--  (f x:map f xs) ++ map f ys
-- == {- cat の定義 -}
--  f x:(map f xs ++ map f ys)
--
-- Theorem: map f (concat xss) == concat (map (map f) xss)
--
-- base case: xss == [] {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  map f (concat [])
-- == {- concat の定義 -}
--  map f (foldr (nil, cat) [])
-- == {- foldr の定義 -}
--  map f []
-- == {- map(foldr) の定義 -}
--  []
--
-- base case: xss == [] {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  concat (map (map f) [])
-- == {- map(folr) の定義 -}
--  concat []
-- == {- concat の定義 -}
--  foldr (nil, cat) []
-- == {- foldr の定義 -}
--  []
--
-- base case: xss == (xs:xss) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  map f (concat (xs:xss)
-- == {- concat の定義 -}
--  map f (foldr (nil, cat) (xs:xss))
-- == {- foldr の定義 -}
--  map f (cat (xs, foldr (nil, cat) xss))
-- == {- cat の定義と concat の定義から foldr を concat に戻す -}
--  map f (xs ++ concat xss)
-- == {- Lamma から -}
--  map f xs ++ map f (concat xss)
-- == {- 帰納法 -}
--  map f xs ++ concat (map (map f) xss)
--
-- base case: xss == (xs:xss) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  concat (map (map f) (xs:xss))
-- == {- map(foldr) の定義 -}
--  concat (map f xs: map (map f) xss)
-- == {- concat の定義 -}
--  foldr (nil, cat) (map f xs: map (map f) xss)
-- == {- foldr の定義と concat の定義から foldr を concat に戻す -}
--  cat (map f xs, concat (map (map f) xss))
-- == {- cat は ++ -}
--  map f xs ++ concat (map (map f) xss)
--
-- 2. listl (listl f) . inits == inits . listl f
--        where inits = foldl ([nil], f)
--                 where f (snoc (xs, x), a) = snoc (snoc (xs, x), snoc (x, a))
inits :: ListL a -> ListL (ListL a)
inits = foldl (c, f)
  where c = Snoc (SNil, SNil)
        f :: (ListL (ListL a), a) -> ListL (ListL a)
        f (yys@(Snoc (ys, y)), z) = Snoc (yys, Snoc (y, z))

lastL :: ListL a -> a
lastL (Snoc (xs, x)) = x
--
-- Lamma: lastL (inits xs) == xs
--
-- base case (xs = [])
-- ~~~~~~~~~~~~~~~~~~~~
--  lastL (inits [])
-- == {- inits の定義 -}
--  lastL [[]]
-- == {- lastL の定義 -}
--  []
--
-- inductive case (xs = xs>:x)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  lastL (inits (xs>:x))
-- == {- inits の定義 -}
--  lastL (foldl ([nil], g) (xs>:x)) where g (ys>:y, z) = (ys>:y) >: (y>:z)
-- == {- foldl の定義 -}
--  lastL (g (inits xs, x))
-- == {- g の定義 -}
--  lastL (inits xs >: (y>:x)) where y = last (inits xs)
-- == {- 帰納法の仮定 -}
--  lastL (inits xs >: (xs>:x))
-- == {- lastL の定義 -}
--  xs>:x
--
-- base case xs == [] {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--  listl (listl f) (inits [])
-- == {- inits(foldl)の定義 -}
--  listl (listl f) [[]]
-- == {- Snocリストの構成 -}
--  listl (listl f) (Snoc ([],[]))
-- == {- listl の定義 -}
--  foldl (SNil, g) (Snoc ([],[])) where g (xs, x) = Snoc (xs, f x)
-- == {- foldl の定義 -}
--  g ([], [])
-- == {- g の定義 -}
--  Snoc ([], listl f [])
-- == {- listl の定義 -}
--  Snoc ([], [])
-- == {- Snoc構成 -}
--  [[]]
--
-- base case xs == [] {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--  inits (listl f [])
-- == {- listl の定義 -}
--  inits []
-- == {- inits の定義 -}
--  [[]]
--
-- inductive case xs == Snoc (xss, xs) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  listl (listl f) (inits (xs:x))
-- == {- inits の定義 -}
--  listl (listl f) (foldl ([nil], g) (xs>:x)) where g (Snoc (ys, y), z) = Snoc (Snoc (ys, y), Snoc (y, z))
-- == {- foldl の定義 -}
--  listl (listl f) (g (inits xs, x))
-- == {- g の定義 -}
--  listl (listl f) (inits xs >: (y >: x)) where y = last (inits xs)
-- == {- Lemma: lastL . inits == id -}
--  listl (listl f) (inits xs >: (xs >: x))
-- == {- listl の定義 -}
--  listl (listl f) (inits xs) >: (listl f (xs >: x))
-- == {- 帰納法の仮定とlistlの定義 -}
--  inits (listl f xs) >: (listl f xs >: f x)
--
-- inductive case xs == Snoc (xss, xs) {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  inits (listl f (xs>:x))
-- == {- listl の定義 -}
--  inits (foldl (SNil, g) (xs>:x)) where g (xs, x) = xs >: f x
-- == {- foldl の定義 -}
--  inits (g (listl f xs, x))
-- == {- g の定義 -}
--  inits (listl f xs >: f x)
-- == {- inits の定義 -}
--  foldl ([nil], g) (listl f xs >: f x) where g (ys >: y, z) = (ys >: y) >: (y >: z)
-- == {- foldl の定義 -}
--  g (inits (listl f xs), f x)
-- == {- g の定義 -}
--  inits (listl f xs) >: (y >: f x) where y = last (inits (listl f xs))
-- == {- Lemma: lastL . inits == id -}
--  inits (listl f xs) >: (listl f xs >: f x)
--
-- 3. listr f . reverse == reverse . listr f
--
reverse :: ListR a -> ListR a
reverse = foldr (Nil, append)
  where append (a, x) = snocr (x, a)
--
-- base case (xs = []) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--  list f (reverse [])
-- ==
--  listl f []
-- ==
--  []
--
-- base case (xs = []) {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--  reverse (listf f [])
-- ==
--  reverse []
-- ==
--  []
--
-- inductive case (xs = x:xs) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  listr f (reverse (x:xs))
-- == {- reverse の定義 -}
--  listr f (foldr ([], append) (x:xs))
-- == {- foldr の定義 -}
--  listr f (append (x, reverse xs))
-- == {- append(snocr) の定義 -}
--  listr f (reverse xs ++ [x])
-- == {- Lemma: map f (xs ++ ys) == map f xs ++ map f ys -}
--  listr f (reverse xs) ++ listr f [x]
-- == {- 帰納法とlistrの定義 -}
--  reverse (listr f xs) ++ [f x]
--
-- inductive case (xs = x:xs) {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  reverse (listr f (x:xs))
-- == {- listr の定義 -}
--  reverse (f x:listr f xs)
-- == {- reverse の定義 -}
--  foldr ([], append) (f x:listr f xs)
-- == {- foldr の定義 -}
--  append (f x, reverse (listr f xs))
-- == {- append(snocr)の定義 -}
--  reverse (listr f xs) ++ [f x]
--
-- 4 . listr (cross (f, g)) . zip == zip . cross (listr f, listr g)
--
-- base case (xs = []) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--  listr (cross (f, g)) (uncurry zip ([], ys))
-- == {- zip の定義 -}
--  listr (cross (f, g)) []
-- == {- listr の定義 -}
--  []
--
-- base case (xs = []) {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--  uncurry zip (cross (listr f, listr g) ([], ys))
-- == {- cross の定義 -}
--  uncurry zip (listr f [], listr g ys)
-- == {- listr の定義 -}
--  uncurry zip ([], listr g ys)
-- == {- zip の定義 -}
--  []
--
-- inductive case (xs = x:xs, ys = []) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  listr (cross (f, g)) (uncurry zip (x:xs, []))
-- ==
--  listr (cross (f, g)) (zip (x:xs) [])
-- ==
--  listr (cross (f, g)) (foldr (c, h) (x:xs) []) where c ys = [], h (x, f) [] = [], h (x, f) (y:ys) = (x,y):f ys
-- ==
--  listr (cross (f, g)) (h (x, zip xs) [])
-- ==
--  listr (cross (f, g)) []
-- ==
--  []
-- inductive case (xs = x:xs, ys = []) {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  uncurry zip (cross (listr f, listr g) (x:xs, []))
-- ==
--  uncurry zip (listr f (x:xs), listr g [])
-- ==
--  uncurry zip (listr f (x:xs), [])
-- ==
--  zip (listr f (x:xs)) []
-- ==
--  zip (f x:listr f xs) []
-- ==
--  foldr (c, h) (f x:listr f xs) [] where c ys = [], h (x, f) [] = [], h (x, f) (y:ys) = (x,y):f ys
-- ==
--  h (f x, zip (listr f xs)) []
-- ==
--  []
--
-- inductive case (xs = x:xs, ys = y:ys) {lhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  listr (cross (f, g)) (uncurry zip (x:xs, y:ys))
-- ==
--  listr (cross (f, g)) (zip (x:xs) (y:ys))
-- ==
--  listr (cross (f, g)) (foldr (c, h) (x:xs) (y:ys)) where c ys = [], h (x, f) [] = [], h (x, f) (y:ys) = (x,y):f ys
-- ==
--  listr (cross (f, g)) (h (x, zip xs) (y:ys))
-- ==
--  listr (cross (f, g)) ((x,y):zip xs ys)
-- ==
--  cross (f, g) (x, y):listr (cross (f, g)) (zip xs ys)
-- ==
--  (f x, g y):listr (cross (f, g)) (zip xs ys)
--
-- inductive case (xs = x:xs, ys = y:ys) {rhs}
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  uncurry zip (cross (listr f, listr g) (x:xs, y:ys))
-- ==
--  uncurry zip (listr f (x:xs), listr g (y:ys))
-- ==
--  uncurry zip (listr f (x:xs), listr g (y:ys))
-- ==
--  zip (listr f (x:xs)) (listr g (y:ys))
-- ==
--  zip (f x:listr f xs) (g y:listr g ys)
-- ==
--  foldr (c, h) (f x:listr f xs) (g y:listr g ys) where c ys = [], h (x, f) [] = [], h (x, f) (y:ys) = (x,y):f ys
-- ==
--  h (f x, zip (listr f xs)) (g y:listr g ys)
-- ==
--  (f x, g y):zip (listr f xs) (listr g ys)
-- ==
--  (f x, g y):uncurry zip (listr f xs, listr g ys)
-- ==
--  (f x, g y):uncurry zip (cross (listr f, listr g) (xs, ys))
-- ==
--  (f x, g y):listr (cross (f, g)) (uncurry zip (xs, ys))
-- ==
--  (f x, g y):listr (cross (f, g)) (zip xs ys)


-- | Ex 1.18
--                    foo             xs      y
-- Tree (A * B) <------------------ ListR A * B             A       B
--      |                                |                  |       |
--      | Tree (f, g)                    | ListR f * g      |f      |g
--      |                                |                  |       |
--      v                                v                  v       v
-- Tree (C * D) <------------------ LIstR C * D             C       D
--                    foo
--
--  tree (cross (f, g))  . foo == foo . cross (listr f, g)
--

-- | Ex 1.19
--                foo
-- ListL A <--------------- GTree A          A
--    |                        |             |
--    |ListL f                 |Gtree f      |f
--    |                        |             |
--    v                        v             v
-- ListL B <--------------- GTree B          B
--                foo
--
--  listl f . foo == foo . gtree f
--
--                      Node
--  GTree a <------------------------------ a * ListL (GTree a)            a
--      |                                        |                         |
--      | u = (|node . (f * id)|)                | 1 * listl u             |f
--      |                                        |                         |
--      v    Node                     f * id     v                         v
--  GTree b <---- b * ListL (GTree b) ----- a * ListL (GTree b)            b
--          <------------------------------
--               Node . (f * id)
--
gtree :: (a -> b) -> GTree a -> GTree b
gtree f = foldg (Node . cross (f, id))

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) (x, y) = (f x, g y)
