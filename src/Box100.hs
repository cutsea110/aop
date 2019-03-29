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

windUp []  = error "Ooops!"
windUp [x] = x
windUp xs  = windUp (windUp1 xs)

windUp1 xs = zipWith bin xs (tail xs)

step l cs r = zipWith bin ([l]++cs) (cs++[r])

zipWind [] cs [] = windUp cs
zipWind [] cs (r:rs) = zipWind [] (windUp1 (cs ++ [r])) rs
zipWind (l:ls) cs [] = zipWind ls (windUp1 ([l] ++ cs)) []
zipWind (l:ls) cs (r:rs) = zipWind ls (windUp1 ([l] ++ cs ++ [r])) rs

mkNexus ls rs = zipWind ls' [] rs'
  where
    (ls', rs') = (P.map tip ls, P.map tip rs)

calc :: Num t => Tree t -> t
calc = histo psi
  where
    psi :: Num t => TreeF t (Cofree (TreeF t) t) -> t
    psi (Tip a)   = a
    psi (Bin x y) = extract x + extract y


rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]
