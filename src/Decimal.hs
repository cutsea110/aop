module Decimal where

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show, Eq)
data DigitPlus = DP1 | DP2 | DP3 | DP4 | DP5 | DP6 | DP7 | DP8 | DP9 deriving (Show, Eq)
data Decimal = Wrap DigitPlus | Snoc Decimal Digit deriving (Show, Eq)

foldDec :: (DigitPlus -> t, t -> Digit -> t) -> Decimal -> t
foldDec (f, g) = u
  where
    u (Wrap dp) = f dp
    u (Snoc dc d) = g (u dc) d

unfoldDec psi d = case psi d of
  Left dp       -> Wrap dp
  Right (dc, d) -> Snoc (unfoldDec psi dc) d
