module Dyna where

data Nat = Z | S Nat deriving Show
-- data Maybe x = Nothing () | Just x
-- F(X) = 1 + X
--
--        [zero, succ]
--    N <--------------- 1 + N
--    |                    |
--  u |                    | id + u
--    v                    v
--    X <--------------- 1 + X
--           [c, f]
--
foldn (c, f) = u
  where u Z     = c
        u (S n) = f (u n)

--
--           out
--    N ---------------> 1 + N
--    ^                    ^
--  v |                    | id + v
--    |                    |
--    X ---------------> 1 + X
--           psi
--
unfoldn psi = v
  where v x = case psi x of
          Nothing -> Z
          Just x  -> S (v x)


data List a = Nil | Cons a (List a) deriving Show
-- data ListF x = NilF () | ConsF Int x
-- Fa(X) = 1 + a * X
--
--        [nil, cons]
--    Ta <--------------- 1 + a * Ta
--    |                    |
--  u |                    | id + id_a * u
--    v                    v
--    X  <--------------- 1 + a * X
--           [c, f]
--
fold (c, f) = u
  where u Nil = c
        u (Cons x xs) = f x (u xs)

--
--          out
--    Ta ---------------> 1 + a * Ta
--    ^                    ^
--  v |                    | id + id_a * v
--    |                    |
--    X  ---------------> 1 + a * X
--           psi
--
unfold psi = v
  where v x = case psi x of
          Nothing -> Nil
          Just (a, x) -> Cons a (v x)
