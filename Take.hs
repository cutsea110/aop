{-# LANGUAGE LambdaCase #-}
module Test where

import Prelude hiding (take)

data Nat = Z | S Nat deriving (Show, Eq, Ord)
{--
on :: Eq a => (a -> Bool) -> (b -> a) -> b -> Bool
(pred `on` acc) x = pred (acc x)

take n = map snd . takeWhile ((<=n) `on` fst) . zip [1..]
--}
foldn :: (a, a -> a) -> Nat -> a
foldn (z, s) = u
  where
    u = \case
      Z -> z
      S n -> s (u n)

plus n = foldn (n, S)
mult n = foldn (Z, plus n)
expr n = foldn (S Z, mult n)

para :: (a, (a, Nat) -> a) -> Nat -> a
para (c, g) = u
  where
    u = \case
      Z -> c
      S n -> g (u n, n)

