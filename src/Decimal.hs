{-# LANGUAGE NPlusKPatterns #-}
module Decimal where

import Prelude hiding (exp, pred, divMod)
import Data.Char (chr, ord)
import ExtEuclid (psi)

val :: Int -> [Int] -> Int
val b = foldl phi 0
  where
    phi n d = n * b + d

val' :: Int -> Int -> [Int]
val' b m = f (m, [])
  where
    f (m, xs)
      | m < b     = m:xs
      | otherwise = f (m `div` b, m `mod` b:xs)

bin, oct, dec, hex :: [Int] -> Int
bin = val 2
oct = val 8
dec = val 10
hex = val 16

bin', oct', dec', hex' :: Int -> [Int]
bin' = val' 2
oct' = val' 8
dec' = val' 10
hex' = val' 16

toChar :: Int -> Char
toChar n
  | 0  <= n && n <= 9  = chr (ord '0' + n)
  | 10 <= n && n <= 15 = chr (ord 'a' + n - 10)


data Nat = Z | S Nat deriving (Show, Eq, Ord)
foldn c f = u
  where u Z = c
        u (S n) = f (u n)

toInt :: Nat -> Int
toInt = foldn 0 (1+)

unfoldn psi = v
  where v x = case psi x of
          Nothing -> Z
          Just x' -> S (v x')

fromInt :: Int -> Nat
fromInt = unfoldn psi
  where psi 0     = Nothing
        psi (n+1) = Just n

pred :: Nat -> Nat
pred Z     = Z
pred (S n) = n

sub :: Nat -> Nat -> Nat
sub a = foldn a pred

-- | return values is (div, mod)
--   [WARN] first argument is denominator, second is numerator
divMod :: Nat -> Nat -> (Nat, Nat)
divMod b = foldn (Z, Z) (succ b)
  where
    -- this is the essence of divMod
    succ b (q, r)
      | S r == b  = (S q, Z)
      | otherwise = (q, S r)

add a  = foldn a S
mult a = foldn Z (add a)
exp a  = foldn (S Z) (mult a)

divide b = fst . divMod b
modulo b = snd . divMod b

even' :: Nat -> Bool
even' = foldn True not

odd' :: Nat -> Bool
odd' = foldn False not

data Bit = O | I deriving (Eq, Show)
data Bin = Nil | Snoc Bin Bit deriving (Eq, Show)
foldbin c f = u
  where u Nil = c
        u (Snoc b x) = f (u b) x

unfoldbin psi = v
  where v x = case psi x of
          Nothing -> Nil
          Just (x', y) -> Snoc (v x') y

convert :: Bin -> Nat
convert = foldbin zero shift
  where zero = Z
        shift n d = (S (S Z) `mult` n) `add` case d of
          O -> Z
          I -> S Z

convert' :: Nat -> Bin
convert' = unfoldbin psi
  where
    psi :: Nat -> Maybe (Nat, Bit)
    psi n = case S (S Z) `divMod` n of
      (Z,  Z)   -> Nothing
      (Z,  S Z) -> Just (Z, I)
      (n', Z)   -> Just (n', O)
      (n', S Z) -> Just (n', I)

-- | high speed exponentiation
exp' :: Nat -> Nat -> Nat
exp' base = foldbin one (op base) . convert'
  where one = S Z
        op a n d
          | d == O    = n `mult` n
          | otherwise = a `mult` (n `mult` n)

exp'' :: Int -> Int -> Int
exp'' a b
  | b == 0    = 1
  | otherwise = op a (exp'' a (b `div` 2), b `mod` 2)
    where op a (n, d)
            | d == 0    = n * n
            | otherwise = a * (n * n)

modulo'' :: Int -> Int -> Int
modulo'' b a
  | a == 0 = 0
  | even a = op b (modulo'' b (a `div` 2), 0)
  | odd  a = op b (modulo'' b (a `div` 2), 1)
  where op b (r, d)
          | n >= b    = n - b
          | otherwise = n
          where n = 2 * r + d
