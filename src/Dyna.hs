{-# LANGUAGE NPlusKPatterns #-}
module Dyna where

import Debug.Trace (trace)

f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}"
         in trace msg v

-- dyna == histo . ana

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

data Nat = Z | S Nat deriving Show

out :: Nat -> Maybe Nat
out Z     = Nothing
out (S n) = Just n

--        [Z, S]
--  Nat <----------- 1 + Nat
--    |               |
--   u|               | id + u
--    V               V
--    X <----------- 1 + X
--        [c, f]

-- F(X) = 1 + X
-- uF = Nat
foldn (c, f) = u
  where
    u Z = c
    u (S n) = f (u n)

toInt :: Nat -> Int
toInt = foldn (0, (+1))

--        out
--  Nat -----------> 1 + Nat
--    A               A
--   v|               | id + v
--    |               |
--    X -----------> 1 + X
--        psi
unfoldn psi = v
  where
    v x = case psi x of
      Nothing -> Z
      Just x' -> S (v x')

fromInt :: Int -> Nat
fromInt = unfoldn psi
  where psi 0     = Nothing
        psi (n+1) = Just n


--    ....  3  2  1  0
--          S  S  S  Z
--          |  |  |  |
--          A  A  A  A


--       Nat <------------------------ F(Nat)
--       / |                            |
--    h / u|                            |Fu
--     v   V                            V
--   A <- vF* <---- A * F(vF*) <------ F(vF*)
--      e     inF*             <phi, id>
--
-- F*(X) = A * F(X)  == A * (1 + X) = A + A * X
-- vF* : fixed point of F* ==> NoEmptyList

-- ListF(X) = 1 + A * X
-- List A = 1 + A * List A
data NEL a = Unit a | Cons a (NEL a) deriving Show

hd :: NEL a -> a
hd (Unit x)   = x
hd (Cons x _) = x

tl :: NEL a -> Maybe (NEL a)
tl (Unit _)    = Nothing
tl (Cons _ xs) = Just xs

histo :: Show a => (Maybe (NEL a) -> a) -> Nat -> a
histo phi = hd . u
  where u n = maybe (Unit val) (Cons val) n'
          where n' = fmap u (out n)
                val = phi $? n'

dyna phi psi = histo phi . unfoldn psi

fib' = dyna phi psi
  where
    phi Nothing = 0
    phi (Just x) = hd x + maybe 1 hd (tl x)

    psi 0 = Nothing
    psi (n+1) = Just n
