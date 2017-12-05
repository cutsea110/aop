{-# LANGUAGE LambdaCase #-}
module Test where

import Prelude hiding (take, drop, splitAt)

pair (f, g) x = (f x, g x)

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

data Nat = Z | S Nat deriving (Show, Eq, Ord)

foldn :: (a, a -> a) -> Nat -> a
foldn (z, s) = u
  where
    u = \case
      Z -> z
      S n -> s (u n)

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn phi = v where v x = maybe Z (S . v) (phi x)

toNat :: Int -> Nat
toNat = unfoldn phi where phi n = if n <= 0 then Nothing else Just (n-1)
fromNat :: Nat -> Int
fromNat = foldn (0, (1+))

plus n = foldn (n, S)
mult n = foldn (Z, plus n)
expr n = foldn (S Z, mult n)
fact = paran (S Z, f) where f (m, n) = mult n (S m)

paran :: (b, (Nat, b) -> b) -> Nat -> b
paran (c, g) = zygon _In phi
  where
    _In = maybe Z S -- alpha
    phi = maybe c g

zygon :: (Maybe a -> a) -> (Maybe (a, b) -> b) -> Nat -> b
zygon f phi = snd . u
  where
    u = foldn (p Nothing, p . Just)
    p = pair (f . fmap fst, phi)

take, drop :: Nat -> [a] -> [a]
(take, drop) = (fmap fst . splitAt, fmap snd . splitAt)

splitAt :: Nat -> [a] -> ([a], [a])
splitAt n xs = zygon f phi n
  where
    f = maybe xs safeTail
    phi = maybe ([], xs) g
    g (xs, (ys, zs)) = (foldr (\z _ -> ys ++ [z]) ys xs, safeTail zs)
