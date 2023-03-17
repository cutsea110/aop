{-# LANGUAGE  DeriveFunctor #-}
module NonEmptyListAsHist where

import Prelude hiding (foldr, subtract)

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
                    deriving (Show, Functor)

out' :: NonEmptyList a -> Either a (a, NonEmptyList a)
out' (Unit x)    = Left x
out' (Cons x xs) = Right (x, xs)

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
histo phi = v
  where
    v = phi . fmap u . out
    u x = maybe (Unit a) (Cons a) b
      where (a, b) = (v x, fmap u (out x))

extract :: NonEmptyList a -> a
extract x = case out' x of
  Left  a      -> a
  Right (a, _) -> a

subtract :: NonEmptyList a -> Maybe (NonEmptyList a)
subtract x = case out' x of
  Left a       -> Nothing
  Right (_, b) -> Just b

fib :: Nat -> Int
fib = histo phi
  where
    phi Nothing = 0
    phi (Just x) = f1 x + f2 x
    f1 :: NonEmptyList Int -> Int
    f1 x = extract x
    f2 :: NonEmptyList Int -> Int
    f2 x = case subtract x of
      Nothing -> 1
      Just y  -> extract y
