module Decimal where

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show, Eq)
data DigitPlus = DP1 | DP2 | DP3 | DP4 | DP5 | DP6 | DP7 | DP8 | DP9 deriving (Show, Eq)
data Decimal = Wrap DigitPlus | Snoc Decimal Digit deriving (Show, Eq)

foldDec :: (DigitPlus -> t, (t, Digit) -> t) -> Decimal -> t
foldDec (f, g) = u
  where
    u (Wrap dp) = f dp
    u (Snoc dc d) = g (u dc, d)

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

plusN x y = foldn (x, S) y
multN x y = foldn (Z, plusN x) y

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

plusNP x y = foldnp (Spl x, Spl) y
multNP x y = foldnp (x, plusNP x) y

embed :: DigitPlus -> NatPlus
embed DP1 = One
embed DP2 = Spl (embed DP1)
embed DP3 = Spl (embed DP2)
embed DP4 = Spl (embed DP3)
embed DP5 = Spl (embed DP4)
embed DP6 = Spl (embed DP5)
embed DP7 = Spl (embed DP6)
embed DP8 = Spl (embed DP7)
embed DP9 = Spl (embed DP8)

val = foldDec (embed, op)
  where
    op :: (NatPlus, Digit) -> NatPlus
    op (n, d) = undefined
