{-# LANGUAGE LambdaCase #-}
module Test where

import FixPrime

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

paran :: (a, (a, Nat) -> a) -> Nat -> a
paran (c, g) = u
  where u = \case
          Z -> c
          S n -> g (u n, n)

fact = paran (S Z, f)
  where f (m, n) = mult m (S n)



zygon :: (Maybe a -> a) -> (Maybe (a, b) -> b) -> Nat -> b
zygon f phi = snd . u
  where
    u Z = (f Nothing, phi Nothing)
    u (S n) = (f (Just (fst x)), phi (Just x))
      where
        x = u n
