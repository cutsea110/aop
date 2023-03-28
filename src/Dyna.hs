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

--
-- Histomorphism
--

-- F(X) = 1 + X  <= F == Maybe
--
-- A * F(X) = A * (1 + X) = A + A * X
--
-- F*(X) = A + A * X
--
data NonEmptyList a = Unit a | Node a (NonEmptyList a) deriving Show
-- data F a x = Unit a | Node a x
--
--        [unit, node]
--   Ta <--------------- a + a * Ta
--    |                    |
--  u |                    | id + u
--    v                    v
--    X <--------------- a + a * X
--           [c, f]
--

out :: Nat -> Maybe Nat
out Z     = Nothing
out (S n) = Just n

--                     out
--         N ------------------------> 1 + N
--       / |                             |
--    h /  | u                           | id + u = Fu
--     /   |                             |
--    v    v                             v
--   A <-- L(A) <--- A + A * L(A) <--- 1 + L(A)
--       e
hist :: (Maybe (NonEmptyList a) -> a) -> Nat -> a
hist phi = hd . u
  where u n = maybe (Unit v) (Node v) m
          where
            m = fmap u $ out n
            v = phi m

hd :: NonEmptyList a -> a
hd (Unit x)   = x
hd (Node x _) = x

tl :: NonEmptyList a -> Maybe (NonEmptyList a)
tl (Unit _)   = Nothing
tl (Node _ x) = Just x

phi :: Maybe (NonEmptyList Int) -> Int
phi Nothing  = 0
phi (Just x) = hd x + maybe 1 hd (tl x)

psi :: Int -> Maybe Int
psi n | n == 0    = Nothing
      | otherwise = Just (n-1)

dyna f g = hist f . unfoldn g

fib :: Int -> Int
fib = dyna phi psi
