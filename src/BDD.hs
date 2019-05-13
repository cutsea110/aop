{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}
module BDD where

import Prelude hiding (either)

import Control.Applicative (liftA2)
import Data.Function (on)

-- HaskellでSimpathを実装してみる
-- by @yasuabe2613
-- https://qiita.com/yasuabe2613/items/f23b1d644d8aede87d97

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
