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
    u = foldn (c, g)
    c = (f (fmap fst Nothing), phi Nothing)
    g = pair (f . fmap fst . Just, phi . Just)
