{-# LANGUAGE LambdaCase #-}
module Test where

pair (f, g) x = (f x, g x)

data Nat = Z | S Nat deriving (Show, Eq, Ord)

foldn :: (a, a -> a) -> Nat -> a
foldn (z, s) = u
  where
    u = \case
      Z -> z
      S n -> s (u n)

plus n = foldn (n, S)
mult n = foldn (Z, plus n)
expr n = foldn (S Z, mult n)

paran :: (a, (Nat, a) -> a) -> Nat -> a
paran (c, g) = u
  where u = \case
          Z -> c
          S n -> g (n, u n)

fact = paran (S Z, f)
  where f (m, n) = mult m (S n)



zygon :: (Maybe a -> a) -> (Maybe (a, b) -> b) -> Nat -> b
zygon f phi = snd . u
  where
    u = foldn ((f Nothing, phi Nothing), g)
    g = pair (f . Just . fst, phi . Just)
