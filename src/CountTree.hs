{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NPlusKPatterns #-}
module CountTree where

import Control.Arrow
import Data.Bool
import Data.List
import Numeric.Natural
import Data.Functor.Foldable

data Tree = Leaf
          | Tree :^: Tree
          deriving Eq

data TreeF r = LeafF
             | r :^^: r
             deriving Eq

instance Functor TreeF where
  fmap f = \case
    LeafF -> LeafF
    l :^^: r -> f l :^^: f r

type instance Base Tree = TreeF

instance Recursive Tree where
  project = \case
    Leaf    -> LeafF
    l :^: r -> l :^^: r

instance Corecursive Tree where
  embed = \case
    LeafF -> Leaf
    l :^^: r -> l :^: r
