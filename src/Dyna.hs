module Dyna where

data Nat = Z | S Nat deriving Show

out :: Nat -> Maybe Nat
out Z     = Nothing
out (S n) = Just n

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
-- F(X) = 1 + X
-- uF == F(uF) = 1 + uF
-- F*(X) = A * F(X) = A * 1 + X = A + A * X
-- vF* == A + A * vF*
data NEList a = Unit a | Cons a (NEList a) deriving Show

in' :: (a, Maybe (NEList a)) -> NEList a
in' (x, Nothing) = Unit x
in' (x, Just xs) = Cons x xs

hd :: NEList a -> a
hd (Unit x)   = x
hd (Cons x _) = x

tl :: NEList a -> Maybe (NEList a)
tl (Unit _)    = Nothing
tl (Cons _ xs) = Just xs

--
--                          [z, s]
--          N  <----------------------------- 1 + N
--         /|                                   |
--        / |                                   |
--  hist /  | u              A <----+           | id + u
--      /   |                |       \  phi     |
--     /    v                v        +------ , v
--    A <-- [A]+ <---- A * (1 + [A]+) <------ 1 + [A]+
--       e         In*       ^        +-------'
--                           |       /
--                       1 + [A]+ <-+
--
hist phi = hd . u
  where u n = in' (v, m)
          where m = fmap u (out n)
                v = phi m

dyna f g = hist f . unfoldn g

------
fib = dyna phi psi
  where phi :: Maybe (NEList Int) -> Int
        phi Nothing = 0
        phi (Just (Unit _)) = 1
        phi (Just (Cons x xs)) = x + hd xs
        psi i = if i <= 0 then Nothing else Just (i-1)

fib' = hist phi . fromInt
  where phi :: Maybe (NEList Int) -> Int
        phi = maybe 0 (\xs -> hd xs + maybe 1 hd (tl xs))
