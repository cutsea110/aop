{-# LANGUAGE TemplateHaskell, TypeFamilies, KindSignatures #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase, BangPatterns #-}
module Treemap where

import Data.Monoid
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

type Label = String

{- |
           [Leaf,Node] x [Nil,Cons]
   Ta x Fa <------------------ (A + Fa) x (1 + Ta x Fa)
    |   |                         |          |
  u |   | v                       | 1 + v    | 1 + u x v
    |   |                         |          |
    v   v                         v          v
    X x Y  <------------------ (A + Y ) x (1 +  X x Y )
            [f, g] x [c, h]
-}
data Treemap a = Leaf a
               | Node Label (Forest a)
               deriving (Show, Functor, Foldable)

newtype Forest a = Grows [Treemap a] deriving (Show, Functor, Foldable)

makeBaseFunctor ''Treemap
makeBaseFunctor ''Forest

sample :: Treemap Integer
sample = Node "cluster1"
         (Grows
           [ Node "cluster2"
             (Grows
               [ Node "cluster4"
                 (Grows
                   [ Leaf 2
                   , Leaf 3
                   ])
               , Leaf 5
               ])
           , Node "cluster3"
             (Grows
               [ Leaf 10
               , Leaf 5
               ])
           ])
