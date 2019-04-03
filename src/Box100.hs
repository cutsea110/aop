{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Box100 where

import Prelude as P hiding (Functor(..))
import Data.List (map, unfoldr, foldl, foldr)

import DrawMatrix
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

windUp :: Num a => [Cofree (TreeF t) a] -> Cofree (TreeF t) a
windUp []  = error "Ooops!"
windUp [x] = x
windUp xs  = windUp (windUp1 xs)

windUp1 :: Num a => [Cofree (TreeF t) a] -> [Cofree (TreeF t) a]
windUp1 xs = zipWith f xs (tail xs)
  where
    f l r = Cf (In (Hisx (extract l + extract r, Bin (unCf l) (unCf r))))

tip' :: a -> Cofree (TreeF a) a
tip' n = Cf (In (Hisx (n, Tip n)))

zipWind :: Num a => ([a], [Cofree (TreeF a) a], [a]) -> Cofree (TreeF a) a
zipWind ([], cs, []) = windUp cs
zipWind ([], cs, r:rs) = zipWind ([], (windUp1 (cs ++ [tip' r])), rs)
zipWind (l:ls, cs, []) = zipWind (ls, (windUp1 ([tip' l] ++ cs)), [])
zipWind (l:ls, cs, r:rs) = zipWind (ls, (windUp1 ([tip' l] ++ cs ++ [tip' r])), rs)

mkNexus :: Num a => ([a], [a]) -> Cofree (TreeF a) a
mkNexus (ls, rs) = zipWind (ls, [], rs)

winder :: ((a, b) -> c) -> (b, [a]) -> Maybe (c, (c, [a]))
winder f (y, xxs) = case xxs of
  []     -> Nothing
  (x:xs) -> Just (y', (y', xs)) where y' = f (x, y)

windCol :: (Tree a, [Tree a]) -> [Tree a]
windCol = unfoldr (winder (uncurry bin))

nexus cs rrs = unfoldr psi (cs, rrs)
  where
    psi (cs, []) = Nothing
    psi (cs, r:rs) = Just (ps, (ps, rs)) where ps = windCol (r, cs)

foo :: ([a], [a]) -> Tree t -- Tree t == Fix (TreeF t)
foo = ana psi
  where
    psi :: ([a], [a]) -> TreeF t ([a], [a])
    psi = undefined

bar :: Tree t -> a -- Tree t == Fix (TreeF t)
bar = histo phi
  where
    phi :: TreeF t (Cofree (TreeF t) a) -> a
    phi = undefined

rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]

exs :: Fix (Hisx (TreeF t) a) -> [[a]]
exs x = case out x of
  Hisx (_, Tip _)   -> []
  Hisx (_, Bin _ r) -> exs r ++ [exsL x]
  where
    exsL :: Fix (Hisx (TreeF t) a) -> [a]
    exsL x = case out x of
      Hisx (_, Tip _) -> []
      Hisx (n, Bin l _) -> exsL l ++ [n]

calc :: Num a => ([a], [a]) -> [[a]]
calc = exs . unCf . mkNexus
