{-# LANGUAGE NPlusKPatterns, TupleSections, DeriveFunctor #-}
module Euclid where

euclid :: Integral a => a -> a -> a
euclid x 0 = x
euclid x y = euclid y (x `mod` y)

{-
euclid' :: Integral a => a -> a -> a
euclid' x y = v $ pure (x, y)
  where
    v = either id (v . psi)
      where
        psi (x, y) = case x `mod` y of
          0 -> Left y
          m -> Right (y, m)
-}

data NEL a = Unit a | Cons a (NEL a) deriving (Show, Functor)

cata :: (a -> b, (a, b) -> b) -> NEL a -> b
cata (f, g) = u
  where
    u (Unit x)    = f x
    u (Cons x xs) = g (x, u xs)

ana :: (b -> Either a (a, b)) -> b -> NEL a
ana psi = v
  where
    v b = case psi b of
      Left x       -> Unit x
      Right (x, y) -> Cons x (v y)

euclid' :: Integral a => (a, a) -> a
euclid' = cata phi . ana psi
  where
    phi = (id, snd)
    psi (x, 0) = Left x
    psi (x, y) = Right (x, (y, x `mod` y))
