{-# LANGUAGE  DeriveFunctor #-}
module NonEmptyListAsHist where

import Prelude hiding (foldr, subtract)
import Debug.Trace (trace)

f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}"
         in trace msg v

pair :: (a -> b, a -> c) -> a -> (b, c)
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

-- | Histomoriphism
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
histo :: Show a => (Maybe (NonEmptyList a) -> a) -> Nat -> a
histo phi = v
  where
    v = (phi $?) . fmap u . out
    u x = maybe (Unit a) (Cons a) b
      where (a, b) = (v x, fmap u (out x))

-- | Histomorphism
--                             inF
--          uF <--------------------------------------------- F(uF)
--          /|                                                  |
--         / |                                                  |
--  histo /  | u = cata (inF* . <phi, id>)                      | Fu
--       /   |                                                  |
--      /    |                                                  |
--     v     v                                                  v
--    A <--- vF* <-------- F*(vF*) == A * F(uF*) <----------- F(vF*)
--        e         inF*                             <phi, id>
--
histo' :: Show a => (Maybe (NonEmptyList a) -> a) -> Nat -> a
histo' phi = extract . u
  where
    u n = maybe (Unit val) (Cons val) m
      where
        m = fmap u (out n)
        val = phi $? m

extract :: NonEmptyList a -> a
extract (Unit x)   = x
extract (Cons x _) = x

subtract :: NonEmptyList a -> Maybe (NonEmptyList a)
subtract = either (const Nothing) (Just . snd) . out'

dyna :: Show a => (Maybe (NonEmptyList a) -> a) -> (b -> Maybe b) -> b -> a
dyna f g = histo f . unfoldn g

dyna' :: Show a => (Maybe (NonEmptyList a) -> a) -> (b -> Maybe b) -> b -> a
dyna' f g = histo' f . unfoldn g

phi :: Maybe (NonEmptyList Int) -> Int
phi Nothing = 0
phi (Just x) = extract x + maybe 1 extract (subtract x)

psi :: Int -> Maybe Int
psi n = if n == 0 then Nothing else Just (n-1)

fib :: Int -> Int
fib = dyna phi psi

fib' :: Int -> Int
fib' = dyna' phi psi
