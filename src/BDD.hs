{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}
module BDD where

data BDD = Top
         | Bot
         | Node { label   :: Integer
                , branch0 :: BDD
                , branch1 :: BDD
                }
         deriving (Show, Eq)
