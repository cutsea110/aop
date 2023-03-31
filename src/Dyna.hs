module Dyna where


data Nat = Z | S Nat deriving Show

--
-- Cata
--
--
--         [z, s]
-- Nat <----------- 1 + Nat
--   |               |
-- u |               |  id + u
--   v               v
--   X <----------- 1 + X
--         [c, f]
--
foldn (c, f) = u
  where u Z = c
        u (S n) = f (u n)

plus x = foldn (x, S)
times x = foldn (Z, plus x)
exp x = foldn (S Z, times x)
toInt = foldn (0, (1+))

--
-- Ana
--
--
--         out
-- Nat -----------> 1 + Nat
--   ^               ^
-- v |               |  id + v
--   |               |
--   X <----------- 1 + X
--          psi
--
unfoldn psi = v
  where v x = case psi x of
          Nothing -> Z
          Just x' -> S (v x')

fromInt = unfoldn (\x -> if x <= 0 then Nothing else Just (x-1))

--
-- Hist
--
