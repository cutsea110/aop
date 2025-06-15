{-# LANGUAGE NPlusKPatterns, TupleSections #-}
module Euclid where

euclid :: Integral a => a -> a -> a
euclid x 0 = x
euclid x y = euclid y (x `mod` y)

cata :: Integral b => a -> (a -> a) -> b -> a
cata c f = u
  where u 0       = c
        u (n + 1) = f (u n)

euclid' :: Integral a => a -> a -> a
euclid' x y = v $ pure (x, y)
  where
    v = either id (v . psi)
      where
        psi (x, y) = case x `mod` y of
          0 -> Left y
          m -> Right (y, m)
