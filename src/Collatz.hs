{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Collatz where

newtype Fix f = In { out :: f (Fix f) }

type Nat = Fix Maybe

instance Show Nat where
  show (In Nothing) = "(In Nothing)"
  show (In (Just x)) = "(In (Just " ++ show x ++ "))"

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

instance Functor f => Functor (Ano f a) where
  fmap f (Ano x y) = Ano x (fmap f y)

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

type NonEmptyList a = Cf Maybe a

-- epsilon -- the extract function.
ex cf = case out (unCf cf) of
  Ano x _ -> x
-- sub
sub cf = case out (unCf cf) of
  Ano _ mv -> fmap Cf mv -- this fmap is over Maybe functor , in the case of Ano Maybe a.

-- anamorphism over Non Empty List which is Cofree of a fixed point of Ano Maybe a
ana psi = In . fmap (ana psi) . psi
{-
histo phi = phi . fmap (Cf . (ana proj)) . out
  where
    proj a = Ano (histo phi a) (out a)
-}
pair (f, g) x = (f x, g x)

bimap (g, h) (Ano a x) = Ano (g a) (fmap h x)

cata phi = phi . fmap (cata phi) . out

histo phi = ex . cata ap
  where
    ap x = cast $ Ano (phi x) x
    cast :: Functor f => Ano f a (Cf f a) -> Cf f a
    cast = Cf . In . bimap (id, unCf)

psi Nothing = 0
psi (Just n) = f1 n + f2 n
  where
    f1 n = ex n
    f2 n = case sub n of
      Nothing -> 1
      Just n' -> ex n'
