{-# LANGUAGE BangPatterns, NPlusKPatterns #-}
module ExtEuclid where

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
  where psi (x, 0) = Left x
        psi (x, y) = Right (y, z) where z = x `mod` y

{- | Trial Eucmorphism : This is a trivial ;-(

        [id, id]
   N <------------- N + N
   ^                  ^
   |                  |
 v |                  | id + v
   |                  |
 N x N -----------> N + N x N
          psi
-}
euc :: ((a, b) -> Either c (a, b)) -> (a, b) -> c
euc psi (x, y) = case psi (x, y) of
  Left  x'       -> x'
  Right (x', y') -> euc psi (x', y')


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
extEuclid x y = psi x y (1, 0) (0, 1)
  where psi x 0 p@(!p1, !p2) _            = (x, p)
        psi x y p@(!p1, !p2) q@(!q1, !q2) = psi y m q (p |-| d |*| q)
          where (d, m) = x `divMod` y

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

{- |
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

anan :: (a -> Maybe a) -> a -> Int
anan psi = v
  where v x = case psi x of
          Nothing -> 0
          Just x' -> 1 + v x'

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
