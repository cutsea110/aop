{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NPlusKPatterns #-}

-- | ref.) https://scrapbox.io/Haskell-Misc/木の数え上げ by nobsun
module CountTree where

import Control.Arrow
import Control.Comonad.Cofree
import Data.Bool
import Data.List
import Data.List.NonEmpty
import Numeric.Natural
import Data.Functor.Foldable
import Data.Functor.Base hiding (head, tail)

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

splits :: Natural -> [(Natural, Natural)]
splits = para phi
  where
    phi :: (Base Natural (Natural, [(Natural, Natural)])) -> [(Natural, Natural)]
    phi = \case
      Nothing -> [(0, 0)]
      Just (n, ds) -> (0, succ n) : map (first succ) ds

allTrees :: [Tree]
allTrees = concatMap trees [0..]

toIndex :: Tree -> Natural
toIndex = head . flip genericFindIndices allTrees . (==)
  where
    genericFindIndices :: (Integral n) => (a -> Bool) -> [a] -> [n]
    genericFindIndices p xs = [ i | (x, i) <- zip xs [0..], p x ]

fromIndex :: Natural -> Tree
fromIndex = genericIndex allTrees

countTrees :: Natural -> Natural
countTrees = \case
  0 -> 1
  n+1 -> sum [ countTrees l * countTrees r | (l, r) <- splits n ]

downFrom :: Natural -> NonEmpty Natural
downFrom = ana psi
  where
    psi :: Natural -> NonEmptyF Natural Natural
    psi = \case
      0   -> NonEmpty 0 Nothing
      n+1 -> NonEmpty (n+1) (Just n)


count :: NonEmpty Natural -> Natural
count = histo phi
  where
    phi :: NonEmptyF Natural (Cofree (NonEmptyF Natural) Natural) -> Natural
    phi = \case
      NonEmpty 0 _             -> 1
      NonEmpty (n+1) (Just ns) -> sum $ zipWith (*) <*> reverse $ xs
        where
          xs = taking n ns

taking :: Natural -> Cofree (NonEmptyF a) b -> [b]
taking = \case
  0   -> const []
  n+1 -> \case
    x :< NonEmptyF _ Nothing -> [x]
    x :< NonEmptyF _ (Just xs) -> x : taking n xs

trees :: Natural -> [Tree]
trees = mkTrees . downFrom

mkTrees :: NonEmpty Natural -> [Tree]
mkTrees = histo phi
  where
    phi :: NonEmptyF Natural (Cofree (NonEmpty Natural) [Tree]) -> [Tree]
    phi = \case
      NonEmptyF 0 _ -> [Leaf]
      NonEmptyF n (Just ns) -> concat $ zipWith (*^*) <*> reverse $ xs
        where
          xs = taking n ns

(*^*) :: [Tree] -> [Tree] -> [Tree]
ss *^* ts = [ s :^: t | s <- ss, t <- ts ]
