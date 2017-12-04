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

paran :: (b, (Nat, b) -> b) -> Nat -> b
paran (c, g) = zygon _In phi
  where
    _In = maybe Z S -- alpha
    phi Nothing = c
    phi (Just x) = g x


fact = paran (S Z, f)
  where f (m, n) = mult m (S n)



zygon :: (Maybe a -> a) -> (Maybe (a, b) -> b) -> Nat -> b
zygon f phi = snd . u
  where
    u = foldn ((f Nothing, phi Nothing), g)
    g = pair (f . Just . fst, phi . Just)
