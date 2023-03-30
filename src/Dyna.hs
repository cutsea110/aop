{-# LANGUAGE NPlusKPatterns #-}
module Dyna where

import Debug.Trace (trace)

f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}"
         in trace msg v

--             a = In
--     uF <-------------- F(uF)
--      |                  |
-- cata |                  | F cata
--      v                  v
--      X <-------------- F(X)
--               phi

--              out
--     vF --------------> F(vF)
--      ^                  ^
--  ana |                  | F ana
--      |                  |
--      X --------------> F(X)
--               psi

--             a = In
--     uF <-------------- F(uF)
--      |                  |
-- para |                  | F (id * cata)
--      v                  v
--      X <-------------- F(uF * X)
--               phi

--              out
--     vF --------------> F(vF)
--      ^                  ^
--  apo |                  | F (id + apo)
--      |                  |
--      X --------------> F(vF + X)
--               psi

--                   a = In
--           uF <---------------------- F(uF)
--          / |            A  <-----+    |
-- hist phi/  | cata       ^   phi   \   | F cata
--        /   v            |          \  v
--       A <- vF* <--- A * F(vF*) <---- F(vF*)
--          e     In*      | <phi,id> /
--                         v         /
--                      F(vF*) <----+
--                               id
--
-- F*(X) = A * F(X)
-- vF* = A * F(vF*)
--
-- ex.)
--   F  == Maybe ==> F(X) = 1 + X
--  uF  == Nat  Z, S Z, S (S Z) ...
--  F*  == A * Maybe ==> F*(X) = A * F(X) = A * (1 + X) == A + A * X
--  vF* == NonEmptyList A
data Nat = Z | S Nat deriving Show

--             [z, s]
--    Nat <-------------- 1 + Nat
--      |                  |
--   u  |                  | id + u
--      v                  v
--      X <-------------- 1 + X
--             [c, f]
foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) = u
  where u Z = c
        u (S n) = f (u n)

--              out
--    Nat --------------> 1 + Nat
--      ^                  ^
--   v  |                  | id + v
--      |                  |
--      X --------------> 1 + X
--               psi
unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn psi = v
  where v x = case psi x of
          Nothing -> Z
          Just x' -> S (v x')

out :: Nat -> Maybe Nat
out Z     = Nothing
out (S n) = Just n

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g x = (f x, g x)

hd :: NEList a -> a
hd (Unit x) = x
hd (Cons x _) = x

tl :: NEList a -> Maybe (NEList a)
tl (Unit _)   = Nothing
tl (Cons _ xs) = Just xs

data NEList a = Unit a | Cons a (NEList a) deriving Show

--                    [z, s]
--          Nat <---------------------- 1 + Nat
--          / |            A  <-----+    |
-- hist phi/  | u          ^   phi   \   | id + u == fmap u
--        /   v            |          \  v
--       A <- [A]+ <- A * (1 + [A]+) <- 1 + [A]+
--          e     In*      | <phi,id> /
--                         v         /
--                    1 + [A]+ <----+
--                               id
--
hist :: (Maybe (NEList a) -> a) -> Nat -> a
hist phi = hd . u
  where u n = maybe (Unit v) (Cons v) m
          where
            m = fmap u (out n) -- fmap is Maybe's
            v = phi m

dyna :: (Maybe (NEList c) -> c) -> (a -> Maybe a) -> a -> c
dyna f g = hist f . unfoldn g

fib :: Integer -> Integer
fib = dyna (phi $?) psi
  where
    psi 0     = Nothing
    psi (n+1) = Just n
    phi :: Maybe (NEList Integer) -> Integer
    phi mayxs = case mayxs of
      Nothing -> 0
      Just xs  -> case tl xs of
        Nothing -> 1
        Just ys -> hd xs + hd ys

fib' :: Integer -> Integer
fib' = fst . foldn (c, f) . unfoldn psi
  where c = (0, 1)
        f (x, y) = (y, x + y)
        psi 0     = Nothing
        psi (n+1) = Just n
