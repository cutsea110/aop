module ExtEuclid where

import Prelude hiding (gcd)

extEuclid :: Integral a => a -> a -> (a, (a, a))
extEuclid x y = f x y (1, 0) (0, 1)
  where
    f x 0 (m0, n0) _        = (x, (m0, n0))
    f x y (m0, n0) (m1, n1) = f y m (m1, n1) (m0-d*m1, n0-d*n1)
      where (d, m) = x `divMod` y

gcd :: Integral a => a -> a -> a
gcd x 0 = x
gcd x y = gcd y (x `mod` y)

