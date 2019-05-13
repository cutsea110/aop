{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}
module BDD where

import Control.Applicative (liftA2)

-- HaskellでSimpathを実装してみる
-- by @yasuabe2613
-- https://qiita.com/yasuabe2613/items/f23b1d644d8aede87d97

type Node = Int
data Edge = Edge { left :: Node, right :: Node } deriving (Show, Eq, Ord)

edge :: Node -> Node -> Edge
edge = (liftA2 . liftA2) Edge min max

start :: Edge
start = edge 0 1
