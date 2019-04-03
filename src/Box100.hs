{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Box100 where

import Prelude as P hiding (Functor(..))
import Data.List (map, unfoldr)

import DrawMatrix
import FixPrime hiding (map)

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

tip' :: a -> Cofree (TreeF a) a
tip' n = Cf (In (Hisx (n, Tip n)))

binWith' :: (a -> a -> a) -> Cofree (TreeF t) a -> Cofree (TreeF t) a -> Cofree (TreeF t) a
binWith' op l r = Cf (In (Hisx (extract l `op` extract r, Bin (unCf l) (unCf r))))

bin' :: Num a => Cofree (TreeF t) a -> Cofree (TreeF t) a -> Cofree (TreeF t) a
bin' = binWith' (+)

winder :: ((a, b) -> c) -> (b, [a]) -> Maybe (c, (c, [a]))
winder f (y, xxs) = case xxs of
  []     -> Nothing
  (x:xs) -> Just (y', (y', xs)) where y' = f (x, y)

windCol :: Num a => (Cofree (TreeF t) a, [Cofree (TreeF t) a]) -> [Cofree (TreeF t) a]
windCol = unfoldr (winder (uncurry bin'))

nexus :: Num a => ([a], [a]) -> [[Cofree (TreeF a) a]]
nexus (cs, rs) = unfoldr psi (map tip' cs, map tip' rs)
  where
    psi (cs, []) = Nothing
    psi (cs, r:rs) = Just (ps, (ps, rs)) where ps = windCol (r, cs)

calc :: Num a => ([a], [a]) -> [[a]]
calc (cs, rs) = map (map extract) $ nexus (cs, rs)

rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]

main :: IO ()
main = draw' 6 calc cols rows
