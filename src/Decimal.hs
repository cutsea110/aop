{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances
 #-}
module Decimal where

import FixPrime

-- Digit = {0,1,2,3,4,5,6,7,8,9}
newtype Digit = D { unD :: Int } deriving Show
-- Digit+ = {1,2,3,4,5,6,7,8,9}
newtype DigitPlus = DP { unDP :: Int } deriving Show

data DecimalF a x = Wrap DigitPlus | Snoc (x, a) deriving Show
type Decimal = Fix (DecimalF Digit)

instance Show Decimal where
    show (In (Wrap x)) = show x
    show (In (Snoc (x, a))) = show x ++ show a

data NatPlus = One | Succ NatPlus deriving Show

