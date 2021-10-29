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

type Total = Integer
type GenTreemap = Integer -> Treemap Integer
phi :: TreemapF Integer (Cofree (TreemapF Integer) (Total, GenTreemap)) -> (Total, GenTreemap)
phi (LeafF a) = (a, Leaf)
phi (NodeF l ts) = (ttl, genNode)
  where genNode n = Node l (zipWith ($) gs ( fmap (\t -> n*t`div`ttl) ts'))
        (ts', gs) = unzip $ fmap extract ts
        ttl = sum ts'

sample :: Treemap Integer
sample = Node "cluster1"
         [ Node "cluster2"
           [ Node "cluster4"
             [ Leaf 2
             , Leaf 3
             ]
           , Leaf 5
           ]
         , Node "cluster3"
           [ Leaf 10
           , Leaf 5
           ]
         ]
