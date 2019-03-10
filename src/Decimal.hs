module Decimal where

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show, Eq)
data DigitPlus = DP1 | DP2 | DP3 | DP4 | DP5 | DP6 | DP7 | DP8 | DP9 deriving (Show, Eq)
data Decimal = Wrap DigitPlus | Snoc Decimal Digit deriving (Show, Eq)

foldDec :: (DigitPlus -> t, t -> Digit -> t) -> Decimal -> t
foldDec (f, g) = u
  where
    u (Wrap dp) = f dp
    u (Snoc dc d) = g (u dc) d

unfoldDec :: (t -> Either DigitPlus (t, Digit)) -> t -> Decimal
unfoldDec psi = v
  where
    v dc = case psi dc of
      Left dp       -> Wrap dp
      Right (dc', d) -> Snoc (v dc') d

data Nat = Z | S Nat deriving (Show, Eq)

foldn :: (t, t -> t) -> Nat -> t
foldn (c, f) = u
  where
    u Z     = c
    u (S x) = f (u x)

unfoldn :: (t -> Maybe t) -> t -> Nat
unfoldn psi = v
  where
    v x = case psi x of
      Nothing -> Z
      Just x' -> S (v x')

data NatPlus = One | Spl NatPlus deriving (Show, Eq)

foldnp (c, f) = u
  where
    u One = c
    u (Spl x) = f (u x)

unfoldnp psi = v
  where
    v x = case psi x of
      Nothing -> One
      Just x' -> Spl (v x')
