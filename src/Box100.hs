{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Box100 where

import Prelude as P hiding (Functor(..))
import FixPrime

-- | Tree a
data TreeF a x = Tip a | Bin x x deriving (Show)
type Tree a = Fix (TreeF a)

tip :: a -> Tree a
tip = In . Tip
bin :: Tree a -> Tree a -> Tree a
bin l r = In (Bin l r)

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

a = bin (tip 1) (tip 3)
b = bin (tip 2) a
c = bin a (tip 4)
d = bin b c

rows = [1,2,3]
cols = [4,5,6]

step l cs r = zipWith bin ([l]++cs) (cs++[r])

zipWind ls cs rs = merge pre
  where
    merge [x] = x
    merge xxs@(x:xs) = merge $ zipWith bin xxs xs
    pre = foldr (\(l,r) c -> step l c r) cs zs
    zs = zip ls rs

mkNexus ls rs = zipWind ls' [] rs'
  where
    (ls', rs') = (P.map tip ls, P.map tip rs)
