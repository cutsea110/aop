{-# LANGUAGE TemplateHaskell, TypeFamilies, KindSignatures #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase, BangPatterns #-}
module Treemap where

import Control.Comonad.Cofree
import Data.Monoid
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

type Label = String

data Treemap a = Leaf a
               | Node Label [Treemap a]
               deriving (Show, Functor, Foldable)

makeBaseFunctor ''Treemap

extract :: Cofree f a -> a
extract (x :< _) = x

sub :: Cofree f a -> f (Cofree f a)
sub (_ :< xs) = xs

type Total = Double
type GenTreemap = Double -> Treemap Double

phi :: Fractional a => TreemapF a (Cofree (TreemapF a) (a -> Treemap a, a)) -> (a -> Treemap a, a)
phi (LeafF a) = (Leaf, a)
phi (NodeF l ts) = (genNode, ttl)
  where (gs, ts') = unzip $ fmap extract ts
        ttl = sum ts'
        genNode n = Node l (zipWith ($) gs rs)
          where rs = fmap (\t -> n*t/ttl) ts'

normalize :: Double -> Treemap Double -> Treemap Double
normalize size tm = let (g, _ttl) = histo phi tm in g size

sample :: Treemap Double
sample = Node "cluster1"
         [ Node "cluster2"
           [ Node "cluster4"
             [ Leaf 2.0
             , Leaf 3.0
             ]
           , Leaf 5.0
           ]
         , Node "cluster3"
           [ Leaf 10.0
           , Leaf 5.0
           ]
         ]

--------

type Weight = Int
class HasWeight a where
  weight :: a -> Weight


data Tile a = T Weight a deriving Show
data Row a = Row Weight [Either (Tile a) (Col a)] deriving Show
data Col a = Col Weight [Either (Tile a) (Row a)] deriving Show

instance HasWeight (Tile a) where
  weight (T w _) = w
instance HasWeight (Row a) where
  weight (Row w _) = w
instance HasWeight (Col a) where
  weight (Col w _) = w

expected :: Row Double
expected = Row 100 [ Right (Col 40 [ Right (Row 20 [ Left (T 8 2.0)
                                                   , Left (T 12 3.0)
                                                   ])
                                   , Left (T 20 5.0)
                                   ])
                   , Right (Col 60 [ Left (T 40 10.0)
                                   , Left (T 20 5.0)
                                   ])
                   ]
