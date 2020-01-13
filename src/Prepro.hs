{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Prepro where

import Data.Functor.Foldable
import Data.Tree

data TreeF a r = NodeF { rootLabelF :: a, subForestF :: [r] }
  deriving (Functor, Foldable, Traversable)
type instance Base (Tree a) = TreeF a
instance Recursive (Tree a) where project (Node a ts) = NodeF a ts
instance Corecursive (Tree a) where embed (NodeF a ts) = Node a ts

tree :: Tree Integer
tree = Node 2 [Node 1 [Node 3 []], Node 7 [Node 1 [], Node 5 []]]

main = do
  drawTree' tree
  -- 0th layer : *1
  -- 1st layer : *2
  -- 2nd layer : *4
  drawTree' $ prepro (\(NodeF x y) -> NodeF (x*2) y) embed tree

  -- sum with deeper values weighted more
  print $ prepro (\(NodeF x y) -> NodeF (x*2) y) ((+) <$> sum <*> rootLabelF) tree
  
  where
    drawTree' = putStr . drawTree . fmap show

{--
sumAlg :: Num a => ListF a a -> a
sumAlg = \case
  Cons h t -> h + t
  Nil      -> 0
lenAlg :: ListF a Int -> Int
lenAlg = \case
  Cons _ t -> 1 + t
  Nil      -> 0

small :: (Ord a, Num a) =>  ListF a b -> ListF a b
small Nil = Nil
small term@(Cons h t) | h <= 10   = term
                      | otherwise = Nil

sum :: Num a => [a] -> a
sum = cata sumAlg

len :: [a] -> Int
len = cata lenAlg

smallSum :: (Ord a, Num a) => [a] -> a
smallSum = prepro small sumAlg

smallLen :: (Ord a, Num a) => [a] -> Int
smallLen = prepro small lenAlg

--}
