{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NPlusKPatterns #-}

-- | ref.) https://scrapbox.io/Haskell-Misc/木の数え上げ by nobsun
module CountTree where

import Control.Arrow
import Data.Bool
import Data.List
import Numeric.Natural
import Data.Functor.Foldable

data Tree = Leaf
          | Tree :^: Tree
          deriving (Eq, Show)

data TreeF r = LeafF
             | r :^^: r
             deriving (Eq, Show)

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

trees :: Natural -> [Tree]
trees = \case
  0   -> [Leaf]
  n+1 -> [s :^: t | (l, r) <- splits n, s <- trees l, t <- trees r]

splits :: Natural -> [(Natural, Natural)]
splits = para phi
  where
    phi :: (Base Natural (Natural, [(Natural, Natural)])) -> [(Natural, Natural)]
    phi = \case
      Nothing -> [(0, 0)]
      Just (n, ds) -> (0, n) : map (first succ) ds

allTrees :: [Tree]
allTrees = concatMap trees [0..]

toIndex :: Tree -> Natural
toIndex = head . flip genericFindIndices allTrees . (==)
  where
    genericFindIndices :: (Integral n) => (a -> Bool) -> [a] -> [n]
    genericFindIndices p xs = [ i | (x, i) <- zip xs [0..], p x ]

fromIndex :: Natural -> Tree
fromIndex = genericIndex allTrees

test = length $ trees 5
