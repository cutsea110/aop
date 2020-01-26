{-# LANGUAGE InstanceSigs #-}
module CategoryTheory where

import Prelude ()

class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id :: (->) a a
  id a = a

  (.) :: (->) b c -> (->) a b -> (->) a c
  f . g = \x -> f (g x)

