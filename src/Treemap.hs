module Treemap where

type Label = String

{- |
           [Leaf,Node] x [Nil,Cons]
   Ta x Fa <------------------ (A + Fa) x (1 + Ta x Fa)
    |   |                         |          |
  u |   | v                       | 1+v      | 1 + u x v
    |   |                         |          |
    v   v                         v          v
    X x Y  <------------------ (A + Y ) x (1 +  X x Y )
            [f, g] x [c, h]
-}
data Treemap a = Leaf a
               | Node Label (Forest a)
               deriving Show

type Forest a = [Treemap a]

foldt :: (a -> c) -> (b -> c) -> b -> (c -> b -> b) -> Treemap a -> c
foldt f g c h = u
  where u (Leaf x)    = f x
        u (Node l ts) = g (v ts)
        v = foldf f g c h

foldf :: (a -> c) -> (b -> c) -> b -> (c -> b -> b) -> Forest a -> b
foldf f g c h = v
  where u = foldt f g c h
        v []     = c
        v (t:ts) = h (u t) (v ts)

sample :: Treemap Int
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
