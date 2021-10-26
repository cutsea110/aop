{-# LANGUAGE TemplateHaskell, TypeFamilies, KindSignatures #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase, BangPatterns #-}
module ExtEuclid where

{- |
my question and duplode's answer.

ref.) https://stackoverflow.com/questions/69716451/is-this-some-kind-of-morphism-in-the-recursion-schemes
-}

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Delayed a = Done a | Waiting (Delayed a) deriving Show
makeBaseFunctor ''Delayed

----------------------------------------------------------

{- |
>>> delayedGCD
Waiting (Waiting (Done 7))
-}
delayedGCD :: Delayed Integer
delayedGCD = ana psi (14, 35)

{- | as same as the Either's either function
>>> cata (delayed id id) delayedGCD
-}
delayed :: (a -> c) -> (b -> c) -> DelayedF a b -> c
delayed f g = u
  where u (DoneF n)    = f n
        u (WaitingF n) = g n

psi :: Integral i => (i, i) -> DelayedF i (i, i)
psi (x, y) = case x `mod` y of
  0 -> DoneF y
  m -> WaitingF (y, m)

euc :: (a -> DelayedF b a) -> a -> b
euc = hylo (delayed id id)

euclid :: (Integer, Integer) -> Integer
euclid = euc psi

extEuclid :: Integral a => a -> a -> (a, (a, a))
extEuclid x y = euc psi ((x, y), (1, 0), (0, 1))
  where psi ((x, 0), p@(!p1, !p2), _)            = DoneF (x, p)
        psi ((x, y), p@(!p1, !p2), q@(!q1, !q2)) = WaitingF ((y, m), q, p |-| d |*| q)
          where (d, m) = x `divMod` y

modInv :: Integral a => a -> a -> a
modInv g p = (x+g) `mod` g
  where (_, (_, x)) = extEuclid g p

(|*|) :: Integral a => a -> (a, a) -> (a, a)
s |*| (x, y) = (s*x, s*y)
infixl 7 |*|

(|-|) :: Integral a => (a, a) -> (a, a) -> (a, a)
(x1, y1) |-| (x2, y2) = (x1-x2, y1-y2)
infixl 6 |-|


{--------------------------------------------------------------------
{- | ユークリッドの互除法
  252  103  46  11   2   1
  103   46  11   2   1   0
---------------------------------------
   46   11   2   1   0
-}
-- euclid x 0 = x
-- euclid x y = euclid y z where z = x `mod` y
euclid :: Integral a => a -> a -> a
euclid x y = euc psi (x, y)
  where psi (x, y) = if m == 0 then Left y else Right (y, m)
          where m = x `mod` y

{- | Trial Eucmorphism : This is a trivial ;-(
            In
    T <---------------- T + T
    ^                     ^
  v |                     | v + id
    |                     |
    A ----------------> A + T
            psi
-}
euc :: (a -> Either t a) -> a -> t
euc = hylo $ either id id

{- | Trial Dual for Eucmorphism
            In
    T <---------------- T x T
    |                     |
  u |                     | u x id
    v                     v
    A <---------------- A x T
            phi
-}
cue :: ((a, t) -> a) -> t -> a
cue phi = phi . pair (cue phi, id)

{- | 拡張ユークリッドの仕組み

STEP 0

    252  103
-------------------------------------------
252   1    0
103   0    1
div
-------------------------------------------

STEP 1 : div = 2, mod = 46

    252  103        46
-------------------------------------------
252   1    0  1+(-2)*0
103   0    1  0+(-2)*1
       \---\-/   /  /
            \---/--/
div        2 --/
-------------------------------------------

STEP 2 : div = 2, mod = 11

    252  103   46     11
-------------------------------------------
252   1    0    1   0+(-2)*1
103   0    1   -2   1+(-2)*(-2)
            \---\---/  /   /
                 \----/---/
div        2    2 ---/
-------------------------------------------

STEP 3 : div = 4, mod = 2

    252  103   46   11        2
-------------------------------------------
252   1    0    1   -2   1+(-4)*(-2)
103   0    1   -2    5  -2+(-4)*5
                 \---\---/  /   /
                      \----/---/
div        2    2    4 ---/
-------------------------------------------

STEP 4 : div = 5, mod = 1

    252  103   46   11    2    1
-------------------------------------------
252   1    0    1   -2    9  -2+(-5)*9
103   0    1   -2    5  -22   5+(-5)*(-22)
                      \---\---/  /   /
                           \----/---/
div        2    2    4    5 ---/
-------------------------------------------

STEP 5 : div = 2, mod = 0

    252  103   46   11    2    1    0
-------------------------------------------
252   1    0    1   -2    9  -47
103   0    1   -2    5  -22  115
div        2    2    4    5    2
-------------------------------------------
-}
extEuclid :: Integral a => a -> a -> (a, (a, a))
extEuclid x y = euc psi ((x, y), (1, 0), (0, 1))
  where psi ((x, 0), p@(!p1, !p2), _)            = Left (x, p)
        psi ((x, y), p@(!p1, !p2), q@(!q1, !q2)) = Right ((y, m), q, p |-| d |*| q)
          where (d, m) = x `divMod` y
{-
extEuclid x y = psi x y (1, 0) (0, 1)
  where psi x 0 p@(!p1, !p2) _            = (x, p)
        psi x y p@(!p1, !p2) q@(!q1, !q2) = psi y m q (p |-| d |*| q)
          where (d, m) = x `divMod` y
-}

(|*|) :: Integral a => a -> (a, a) -> (a, a)
s |*| (x, y) = (s*x, s*y)
infixl 7 |*|

(|-|) :: Integral a => (a, a) -> (a, a) -> (a, a)
(x1, y1) |-| (x2, y2) = (x1-x2, y1-y2)
infixl 6 |-|

{- |
         [z, s]
   N <------------- 1 + N
   |                  |
 u |                  | id + u
   |                  |
   v                  v
   A <------------- 1 + A
         [c, f]
-}
catan :: a -> (a -> a) -> Int -> a
catan c f = u
  where u 0 = c
        u (n+1) = f (u n)

anan :: (a -> Maybe a) -> a -> Int
anan psi = v
  where v x = case psi x of
          Nothing -> 0
          Just x' -> succ (v x')

{- | para
         [z, s]
   N <------------- 1 + N
   |                  |
 u |                  | id + (u * id)
   |                  |
   v                  v
   A <------------- 1 + (A x N)
         [c, f]
-}
paran :: a -> (a -> Int -> a) -> Int -> a
paran c f = u
  where u 0 = c
        u (n+1) = f (u n) n

{- | apo
         [z, s]
   N <------------- 1 + N
   ^                  ^
   |                  |
 u |                  | id + (u + id)
   |                  |
   A -------------> 1 + (A + N)
         psi
-}
apon :: (a -> Maybe (Either a Int)) -> a -> Int
apon psi = v
  where v x = case psi x of
          Nothing -> 0
          Just (Left  x) -> succ (v x)
          Just (Right y) -> succ y

{- | mod逆元
-- x の逆元 1/x (mod p) は
-- x * (1/x) == 1 (mod p)
-- となるような 1/x なので求めた逆元に x を掛けると mod p で 1 になる。
-- 5 * 8 == 40 == 1 (mod 13)
>>> modInv 13 8
5
>>> modInv 13 7
2
>>> modInv 13 6
11
-}
modInv :: Integral a => a -> a -> a
modInv g p = (x+g) `mod` g
  where (_, (_, x)) = extEuclid g p
--------------------------------------------------------------------}
