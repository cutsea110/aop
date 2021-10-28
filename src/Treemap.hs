{-# LANGUAGE DeriveFunctor #-}
module Treemap where

import Data.Monoid

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
               deriving (Show, Functor)

type Forest a = [Treemap a]

outt :: Treemap a -> Either a (Label, Forest a)
outt (Leaf x) = Left x
outt (Node l fs) = Right (l, fs)

outf :: Forest a -> Maybe (Treemap a, Forest a)
outf []     = Nothing
outf (t:ts) = Just (t, ts)

foldt :: (a -> c) -> (Label -> b -> c) -> b -> (c -> b -> b) -> Treemap a -> c
foldt f g c h = u
  where u (Leaf x)    = f x
        u (Node l ts) = g l (v ts)
        v = foldf f g c h

unfoldt :: (b -> Either a (Label, c)) -> (c -> Maybe (b, c)) -> b -> Treemap a
unfoldt psit psif t = case psit t of
  Left x        -> Leaf x
  Right (l, fs) -> Node l (unfoldf psit psif fs)

foldf :: (a -> c) -> (Label -> b -> c) -> b -> (c -> b -> b) -> Forest a -> b
foldf f g c h = v
  where u = foldt f g c h
        v []     = c
        v (t:ts) = h (u t) (v ts)

unfoldf :: (b -> Either a (Label, c)) -> (c -> Maybe (b, c)) -> c -> Forest a
unfoldf psit psif f = case psif f of
  Nothing -> []
  Just (t, ts) -> unfoldt psit psif t : unfoldf psit psif ts

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
