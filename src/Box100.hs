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

calc :: Num t => Tree t -> t
calc = histo psi
  where
    psi :: Num t => TreeF t (Cofree (TreeF t) t) -> t
    psi (Tip a)   = a
    psi (Bin x y) = extract x + extract y

rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]

exs :: Fix (Hisx (TreeF t) a) -> [a]
exs x = case out x of
  Hisx (n, Tip _)   -> [n]
  Hisx (n, Bin l _) -> n : exs l

{-
λ> draw' 3 tabulation [1,2,3] [4,5,6]
      4   5   6
   ============
 1|   5  10  16
 2|   7  17  33
 3|  10  27  60
-}
