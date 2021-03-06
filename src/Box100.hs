{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}
module Box100 where

import Prelude as P hiding (Functor(..))
import Data.List (map, unfoldr)
import Data.Time

import DrawMatrix
import FixPrime hiding (map)

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

winder :: MonadFail m => ((a, b) -> c) -> (b, [a]) -> m (c, (c, [a]))
winder f (y, xxs) = case xxs of
  []     -> fail "Exhausted"
  (x:xs) -> return (y', (y', xs)) where y' = f (x, y)

windCol :: Num a => (Cofree (TreeF t) a, [Cofree (TreeF t) a]) -> [Cofree (TreeF t) a]
windCol = unfoldr (winder bin')

nexus :: Num a => ([a], [a]) -> [[Cofree (TreeF a) a]]
nexus = unfoldr (winder windCol) . tupply (map tip')

simple :: Num a => (a -> a -> a) -> ([a], [a]) -> [[a]]
simple op (cs, rs) = [[op c r | c <- cs] | r <- rs]

-- normal 100 masu calc training
simplePlus :: Num a => ([a], [a]) -> [[a]]
simplePlus = simple (+)

simpleMinus :: Num a => ([a], [a]) -> [[a]]
simpleMinus = simple (-)

simpleTimes :: Num a => ([a], [a]) -> [[a]]
simpleTimes = simple (*)

simpleDivide :: Integral a => ([a], [a]) -> [[a]]
simpleDivide = simple div

-- accumulative 100 masu calc naive ver.
naive :: ([Int], [Int]) -> [[Int]]
naive (cs, rs) = [[val (i, j) | j <- [0..c']] | i <- [0..r']]
  where
    (c', r') = (length cs - 1, length rs - 1)
    val (0, 0) = rs !! 0 + cs !! 0
    val (0, j) = val (0, j-1) + cs !! j
    val (i, 0) = rs !! i + val (i-1, 0)
    val (i, j) = val (i, j-1) + val (i-1, j)

-- accumulative 100 masu calc using nexus
calc :: Num a => ([a], [a]) -> [[a]]
calc = map (map extract) . nexus

perfCheck :: IO a -> IO NominalDiffTime
perfCheck act = do
  s <- getCurrentTime
  act
  e <- getCurrentTime
  return (diffUTCTime e s)

rows,cols :: [Int]
-- rows = [4,2,5,6,7,1,3,9,3,2]
-- cols = [8,2,4,6,1,8,9,3,1,7]
-- nao's self problems
rows = [8,6,2,7,9,20,12,6,10,9,5,4,1,0]
cols = [0,1,20,3,10,6,7,9,8,2]

main :: IO ()
main = draw' 6 calc cols rows
