{-# LANGUAGE  DeriveFunctor #-}
module NonEmptyListAsHist where

import Prelude hiding (foldr)
import Debug.Trace (trace)

f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}"
         in trace msg v

data Nat = Zero
         | Succ Nat
         deriving Show

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

-- | Histomorphism
--                             inF
--          uF <--------------------------------------------- F(uF)
--          /|                                                  |
--         / |                                                  |
--  histo /  | u = cata (inF* . <phi, id>)                      | Fu
--       /   |                                                  |
--      /    |                                                  |
--     v     v                                                  v
--    A <--- vF* <-------- F*(vF*) == A * F(vF*) <----------- F(vF*)
--        e         inF*                             <phi, id>
--
histo :: Show a => (Maybe (NonEmptyList a) -> a) -> Nat -> a
histo phi = hd . u
  where
    u n = maybe (Unit val) (Cons val) m
      where
        m = fmap u (out n)
        val = phi $? m

hd :: NonEmptyList a -> a
hd (Unit x)   = x
hd (Cons x _) = x

tl :: NonEmptyList a -> Maybe (NonEmptyList a)
tl = either (const Nothing) (Just . snd) . out'

dyna :: Show a => (Maybe (NonEmptyList a) -> a) -> (b -> Maybe b) -> b -> a
dyna f g = histo f . unfoldn g

fib :: Int -> Int
fib = dyna phi psi
  where phi Nothing = 0
        phi (Just x) = hd x + maybe 1 hd (tl x)
        psi n = if n == 0 then Nothing else Just (n-1)
