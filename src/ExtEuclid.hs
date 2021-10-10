module ExtEuclid where

import Prelude hiding (gcd)

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
div        2
-------------------------------------------

STEP 2 : div = 2, mod = 11

    252  103   46     11
-------------------------------------------
252   1    0    1   0+(-2)*1
103   0    1   -2   1+(-2)*(-2)
div        2    2
-------------------------------------------

STEP 3 : div = 4, mod = 2

    252  103   46   11        2
-------------------------------------------
252   1    0    1   -2   1+(-4)*(-2)
103   0    1   -2    5  -2+(-4)*5
div        2    2    4
-------------------------------------------

STEP 4 : div = 5, mod = 1

    252  103   46   11    2    1
-------------------------------------------
252   1    0    1   -2    9  -2+(-5)*9
103   0    1   -2    5  -22   5+(-5)*(-22)
div        2    2    4    5
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
extEuclid x y = f x y (1, 0) (0, 1)
  where
    f x 0 (m0, n0) _        = (x, (m0, n0))
    f x y (m0, n0) (m1, n1) = f y m (m1, n1) (m0-d*m1, n0-d*n1)
      where (d, m) = x `divMod` y

gcd :: Integral a => a -> a -> a
gcd x 0 = x
gcd x y = gcd y (x `mod` y)

