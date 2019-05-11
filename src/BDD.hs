{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}
module BDD where

import Prelude as P hiding (Functor(..))

import FixPrime

-- | Tree a
data TreeF a x = Tip a | Bin x x deriving (Show)
type Tree a = Fix (TreeF a)

tip :: a -> Tree a
tip = In . Tip

bin :: Tree a -> Tree a -> Tree a
bin l r = In (Bin l r)

tip' :: a -> Cofree (TreeF a) a
tip' n = Cf (In (Hisx (n, Tip n)))

bin' :: Num a => (Cofree (TreeF t) a, Cofree (TreeF t) a) -> Cofree (TreeF t) a
bin' (l, r) = Cf (In (Hisx (extract l + extract r, Bin (unCf l) (unCf r))))

instance Show a => Show (Tree a) where
  show (In (Tip x)) = "Tip " ++ show x
  show (In (Bin l r)) = "Bin (" ++ show l ++ ") (" ++ show r ++ ")"

instance Bifunctor TreeF where
  bimap (f, g) (Tip x) = Tip (f x)
  bimap (f, g) (Bin l r) = Bin (g l) (g r)

instance Functor (TreeF a) where
  fmap f = bimap (id, f)

instance ApplicativeBifunctor TreeF where
  biap (Tip f) (Tip x) = Tip (f x)
  biap (Bin f g) (Bin l r) = Bin (f l) (g r)
