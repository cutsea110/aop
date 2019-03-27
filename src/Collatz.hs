module Collatz where

newtype Fix f = In { out :: f (Fix f) }

type Nat = Fix Maybe

z = In Nothing
s n = In (Just n)

foldn :: (t, t -> t) -> Nat -> t
foldn (c, f) = u
  where
    u (In Nothing) = c
    u (In (Just n)) = f (u n)

unfoldn :: (t -> Maybe t) -> t -> Nat
unfoldn psi x = v
  where
  v = case psi x of
    Nothing -> z
    Just x' -> s (unfoldn psi x')

-- utilities
toNat = unfoldn psi
  where
    psi 0 = Nothing
    psi n = Just (n-1)
fromNat = foldn (0, (1+))

-- with Anotation
-- Ano F A == (A, F(-))
-- the case of F = Maybe, Ano Maybe A => (A, Maybe -)
data Ano f a x = Ano a (f x) deriving Show

-- a.k.a. Cofree
-- the case of F = Maybe, Fix (Ano Maybe A) => the fix point of X = F'(X) = (A, Maybe X) ~ A * (1 + X) = A + A * X,
-- so this functor is the non-empty list of A, Cofree Maybe A is non-empty list of A
--
-- we think Nothing is 0, then we can think (Wrap a :: NonEmptyList a) is 0 with annoteted value a,
-- for example, Wrap 3 means a natural number 0 annotetad with 3.
-- you can make the value, for instance,
-- Cf (In (Ano 0 Nothing))
-- Cf (In (Ano 1 (Just (In (Ano 0 Nothing)))))
-- Cf (In (Ano 2 (Just (In (Ano 1 (Just (In (Ano 0 Nothing))))))))
-- .. and so on.
-- This is the isomorphism to a non-empty list.
newtype Cf f a = Cf { unCf :: Fix (Ano f a) }

-- epsilon -- the extract function.
ex cf = case out (unCf cf) of
  Ano x _ -> x
-- sub
sub cf = case out (unCf cf) of
  Ano _ mv -> fmap Cf mv -- this fmap is over Maybe functor , in the case of Ano Maybe a.
