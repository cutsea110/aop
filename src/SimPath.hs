{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}
module SimPath where

import Prelude hiding (either)

import Control.Applicative (liftA2)
import qualified Data.Foldable as Fold
import Data.Function (on)
import Data.Set

-- HaskellでSimpathを実装してみる
-- by @yasuabe2613
-- https://qiita.com/yasuabe2613/items/f23b1d644d8aede87d97

-- Edge
type Node = Int
data Edge = Edge { left :: Node, right :: Node } deriving (Show, Eq, Ord)

edge :: Node -> Node -> Edge
edge = (liftA2 . liftA2) Edge min max

start :: Edge
start = edge 0 1

onBoth :: (a -> a -> b) -> (Node -> a) -> Edge -> b
onBoth f g (Edge l r) = (f `on` g) l r

either :: (Node -> Bool) -> Edge -> Bool
either = onBoth (||)

contains :: Node -> Edge -> Bool
contains = either . (==)

modify :: (Node -> Node) -> Edge -> Edge
modify = onBoth edge

opposite :: Node -> Edge -> Node
opposite n (Edge l r) = if l == n then r else l

isOpen :: Edge -> Bool
isOpen = onBoth (/=) id

find :: Node -> Set Edge -> Maybe Edge
find = Fold.find . contains

connect :: Node -> Node -> Edge -> Edge
connect from to = edge from . opposite to

-- Frontier
type Used  = Set Node
type Edges = Set Edge
data Frontier = Frontier { edges :: Edges, used :: Used } deriving (Show, Eq, Ord)

initial :: Frontier
initial = Frontier (singleton start) empty

modify' :: (Edges -> Edges) -> (Used -> Used) -> Frontier -> Frontier
modify' f g = Frontier <$> f . edges <*> g . used
