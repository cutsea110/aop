{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, TupleSections #-}
module ShortestPathSearch where

import Prelude as P hiding (Functor(..))
import FixPrime

-- | Tree
data TreeF a x = Tip a | Bin (a, x) (a, x) deriving Show
type Tree a = Fix (TreeF a)

tip :: a -> Tree a
tip = In . Tip

bin :: (a, Tree a) -> (a, Tree a) -> Tree a
bin (x, l) (y, r) = In (Bin (x, l) (y, r))

tip' :: a -> Cofree (TreeF a) a
tip' n = Cf (In (Hisx (n, Tip n)))

bin' :: (Ord a, Num a) => ((a, Cofree (TreeF a) a), (a, Cofree (TreeF a) a)) -> Cofree (TreeF a) a
bin' ((x, l), (y, r)) = Cf (In (Hisx (min (extract l + x) (extract r + y), Bin (x, unCf l) (y, unCf r))))

instance Show a => Show (Tree a) where
  show (In (Tip x)) = "Tip " ++ show x
  show (In (Bin (x, l) (y, r))) = "Bin ((" ++ show x ++ "," ++ show l ++ "),(" ++ show y ++ "," ++ show r ++ "))"

instance Bifunctor TreeF where
  bimap (f, g) (Tip x) = Tip (f x)
  bimap (f, g) (Bin (x, l) (y, r)) = Bin (f x, g l) (f y, g r)

instance Functor (TreeF a) where
  fmap f = bimap (id, f)

instance ApplicativeBifunctor TreeF where
  biap (Tip f) (Tip x) = Tip (f x)
  biap (Bin (f, g) (h, i)) (Bin (x, l) (y, r)) = Bin (f x, g l) (h y, i r)

node0 = tip' 0
nodeInf = tip' (1/0)

