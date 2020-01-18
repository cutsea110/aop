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
import Data.List.NonEmpty hiding (reverse, zipWith, zip, map, head)
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
countTrees = count . downFrom

downFrom :: Natural -> NonEmpty Natural
downFrom = ana psi
  where
    psi :: Natural -> NonEmptyF Natural Natural
    psi = \case
      0   -> NonEmptyF 0 Nothing
      n+1 -> NonEmptyF (n+1) (Just n)


count :: NonEmpty Natural -> Natural
count = histo phi
  where
    phi :: NonEmptyF Natural (Cofree (NonEmptyF Natural) Natural) -> Natural
    phi = \case
      NonEmptyF 0 _             -> 1
      NonEmptyF (n+1) (Just ns) -> sum $ zipWith (*) <*> reverse $ xs
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
    phi :: NonEmptyF Natural (Cofree (NonEmptyF Natural) [Tree]) -> [Tree]
    phi = \case
      NonEmptyF 0 _         -> [Leaf]
      NonEmptyF n (Just ns) -> concat $ zipWith (*^*) <*> reverse $ xs
        where
          xs = taking n ns

(*^*) :: [Tree] -> [Tree] -> [Tree]
ss *^* ts = [ s :^: t | s <- ss, t <- ts ]

catalan :: Natural -> Natural
catalan = \case
  0   -> 1
  n+1 -> dwnprd 1 (2*n) (n+2) `div` dwnprd 1 n 2
    where
      dwnprd a m n | m < n     = a
                   | otherwise = dwnprd (a*m) (pred m) n

catalans :: [Natural]
catalans = 1 : zipWith rec [0..] catalans
  where
    rec n cn = (4*n + 2) * cn `div` (n + 2)

toLocalIndex :: Tree -> (Natural, Natural)
toLocalIndex = cata phi
  where
    phi :: TreeF (Natural, Natural) -> (Natural, Natural)
    phi = \case
      LeafF -> (0, 0)
      (m, i) :^^: (n, j) -> (o, k)
        where
          o = succ (m + n)
          k = acc 0 m 0 (m + n) + i * catalan n + j
          acc a 0 _ _ = a
          acc a p q r = acc (a + catalan q * catalan (r - q)) (pred p) (succ q) r
