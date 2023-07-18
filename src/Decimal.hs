{-# LANGUAGE NPlusKPatterns #-}
module Decimal where

import Prelude hiding (exp, pred)
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
divide :: Nat -> Nat -> Nat
a `divide` b = f Z a b
  where f d a b
          | a < b     = d
          | otherwise = f (S d) (a `sub` b) b

add a b = foldn b S a
mult a = foldn Z (add a)
exp a = foldn (S Z) (mult a)
modulo b = foldn Z (succ b)
  where
    succ b a
      | S a == b  = Z
      | otherwise = S a

data Bit = O | I deriving (Eq, Show)
data Bin = Nil | Snoc Bin Bit deriving (Eq, Show)
foldbin c f = u
  where u Nil = c
        u (Snoc b x) = f (u b) x

convert :: Bin -> Nat
convert = foldbin zero shift
  where zero = Z
        shift n d = (S (S Z) `mult` n) `add` case d of
          O -> Z
          I -> S Z

convert' :: Nat -> Bin
convert' = undefined

-- | high speed exponentiation
exp' :: Nat -> Nat -> Nat
exp' base = foldbin one (op base) . convert'
  where one = S Z
        op a n d
          | d == O    = n `mult` n
          | otherwise = a `mult` (n `mult` n)
