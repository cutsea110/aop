module NonEmptyListAsHist where

import Prelude hiding (foldr)

pair :: (t -> a, t -> b) -> t -> (a, b)
pair (f, g) x = (f x, g x)

--           [zero, succ]
--   N <--------------------- 1 + N
--   |                          |
--   | u                        | id + u
--   v                          v
--   X <--------------------- 1 + X
--           [c, f]
--
data Nat = Zero
         | Succ Nat
         deriving Show

foldn :: a -> (a -> a) -> Nat -> a
foldn c f = u
  where u Zero     = c
        u (Succ n) = f (u n)

--           [zero, succ]
--     <---------------------
--   N ---------------------> 1 + N
--   ^          out             ^
--   | v                        | id + v
--   |                          |
--   X ---------------------> 1 + X
--            psi
--
unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn psi = v
  where v x = case psi x of
          Nothing -> Zero
          Just y  -> Succ (v y)

--
--                         F = Maybe
--         outF           uF = Nat    <histo phi, out>
--   uF ---------> F(uF)      uF  ----------------------> A * F(uF) == F*(uF)
--   |               |        |                             |
--   | histo phi     | Fu     | v                           | id * Fv
--   |               |        |                             |
--   v               v        v                             v
--   A  ---------> F(vF*)     vF* ----------------------> A * F(vF*) == F*(vF*)
--         phi                           outF*
--
out :: Nat -> Maybe Nat
out Zero     = Nothing
out (Succ n) = Just n

data NonEmptyList a = Unit a
                    | Cons a (NonEmptyList a)
                    deriving Show
--
--           [unit, cons]
--   Ta <--------------------- a + a * Ta
--   |                          |
--   | u                        | id + u
--   v                          v
--   X  <--------------------- a + a * X
--           [c, f]
--
foldr :: (a -> b) -> (a -> b -> b) -> NonEmptyList a -> b
foldr c f = u
  where u (Unit x) = c x
        u (Cons x y) = f x (u y)

--
--           [unit, cons]
--      <---------------------
--   Ta ---------------------> a + a * Ta
--   ^          out             ^
--   | v                        | id + a * v
--   |                          |
--   X  ---------------------> a + a * X
--           psi
--
unfoldr :: (t -> Either a (a, t)) -> t -> NonEmptyList a
unfoldr psi = v
  where v x = case psi x of
          Left a       -> Unit a
          Right (a, b) -> Cons a (v b)

histo :: (Maybe (NonEmptyList a) -> a) -> Nat -> a
-- out :: (Nat -> Maybe Nat)
-- u :: Nat -> NonEmptyList a
-- fmap u :: Maybe Nat -> Maybe (NonemptyList a)
-- phi :: Maybe (NonemptyList a) -> b(==a)
histo phi = v
  where
    v = phi . fmap u . out
    u = unfoldr f
      where
        f Zero     = Left  (v Zero)
        f (Succ n) = Right (v n, n)
