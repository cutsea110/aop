module Hylo where

import Prelude hiding (exp)

data Nat = Z | S Nat deriving (Show, Eq)

foldn (c, f) = u
  where u Z = c
        u (S n) = f (u n)

plus n = foldn (n, S)

mult n = foldn (Z, plus n)

exp n = foldn (one, mult n)

one = S Z

type Bit = Int -- ただし 0 or 1
data Bin = Nil | Snoc Bin Bit deriving (Show, Eq)

foldbin (c, f) = u
  where u Nil = c
        u (Snoc xs x) = f (u xs) x

convert = foldbin (0, shift)
  where shift n d = 2 * n + d
