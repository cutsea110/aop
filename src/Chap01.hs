{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chap01 where

import Prelude hiding (last, foldr, foldl, take, drop)

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

data ListL a = SNil
             | Snoc (ListL a, a)
             deriving Show

foldl :: (b, (b, a) -> b) -> ListL a -> b
foldl (c, f) = u
  where u SNil = c
        u (Snoc (xs, a)) = f (u xs, a)

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

listl :: (a -> b) -> ListL a -> ListL b
listl f = foldl (SNil, g)
  where g (xs, x) = Snoc (xs, f x)

size :: GTree a -> Integer
size = foldg g
  where g (x, ts) = 1 + sum ts
        sum :: ListL Integer -> Integer
        sum = foldl (c, f)
        c = 0
        f (xs, x) = xs + x
