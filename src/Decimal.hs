module Decimal where

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show, Eq)
data DigitPlus = DP1 | DP2 | DP3 | DP4 | DP5 | DP6 | DP7 | DP8 | DP9 deriving (Show, Eq)
data Decimal = Wrap DigitPlus | Snoc Decimal Digit deriving (Show, Eq)
