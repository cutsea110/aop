{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances
 #-}
module Decimal where

import Prelude hiding (Functor(..))
import FixPrime

-- Digit = {0,1,2,3,4,5,6,7,8,9}
newtype Digit = D { unD :: Int }
instance Show Digit where
    show = show . unD
d :: Int -> Digit
d n | 0 <= n && n < 10 = D n
    | otherwise = error "invalid Digit value"    

-- Digit+ = {1,2,3,4,5,6,7,8,9}
newtype DigitPlus = DP { unDP :: Int }
instance Show DigitPlus where
    show = show . unDP
dp :: Int -> DigitPlus
dp n | 0 < n && n < 10 = DP n
     | otherwise = error "invalid Digit+ value"

data DecimalF a x = Wrap DigitPlus | Snoc (x, a) deriving Show
type Decimal = Fix (DecimalF Digit)

wrap :: Int -> Decimal
wrap = In . Wrap . dp
snoc :: Decimal -> Int -> Decimal
snoc dec n = In . Snoc . cross (id, d) $ (dec, n)

instance Show Decimal where
    show x = case out x of
        Wrap n      -> show n
        Snoc (x, a) -> show x ++ show a

instance Bifunctor DecimalF where
    bimap (f, g) (Wrap n) = Wrap n
    bimap (f, g) (Snoc (x, a)) = Snoc (g x, f a)

instance Functor (DecimalF Digit) where
    fmap f = bimap (id, f)

data NatPlus = One | Succ NatPlus deriving Show
foldn :: (a, a -> a) -> NatPlus -> a
foldn (c, f) One = c
foldn (c, f) (Succ n) = f (foldn (c, f) n)
unfoldn :: (a -> Maybe a) -> a -> NatPlus
unfoldn psi x = case psi x of
    Nothing -> One
    Just x' -> Succ (unfoldn psi x')

plus x = foldn (Succ x, Succ)
mult x = foldn (x, plus x)
nat :: Int -> NatPlus
nat = unfoldn psi
    where
        psi n | n == 1 = Nothing
              | otherwise = Just (n-1)

embed :: DigitPlus -> NatPlus
embed (DP n) | n == 1 = One
             | otherwise = Succ (embed (DP (n-1)))

op :: (Digit, NatPlus) -> NatPlus
op (D n, m) | n == 1 = Succ m
            | otherwise = op (d (n-1), Succ m)

val :: Decimal -> NatPlus
val = cata phi
    where
        phi (Wrap n) = embed n
        phi (Snoc (d, n)) = op (n, d)

