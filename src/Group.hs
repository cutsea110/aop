module Group where

import Data.Function (on)
import Data.List (sortOn, groupBy)
import Data.Monoid (Endo(..))
import qualified Data.Set as Set

import Combinatorial (perms)

type DIM = Int
-- | Symmetric group of degree n
data Sym = Sym DIM [Int] deriving (Eq, Ord, Show)

syms :: DIM -> [Sym]
syms n = Sym n <$> perms [1..n]

apply :: Sym -> Sym -> Sym
apply (Sym n xs) (Sym n' ys)
  | n == n'   = Sym n $ map ((xs!!) . pred) ys
  | otherwise = error "apply: dimensions do not match" 

toEndo :: Sym -> Endo Sym
toEndo = Endo . apply

-- | 逆元
complement :: Sym -> Sym
complement (Sym n xs) = Sym n xs'
  where xs' = map fst . sortOn snd . zip [1..] $ xs

-- | f の g による共役元
covariantOver :: Sym -> Sym -> Sym -> Sym
f `covariantOver` g = apply g . apply f . apply g'
  where g' = complement g

-- | 3次対称群の正規部分群
g3 :: [Sym]
g3 = map (Sym 3) [[1,2,3],[2,3,1],[3,1,2]]

-- | 4次対称群の正規部分群
g4 :: [Sym]
g4 = map (Sym 4) [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]]

-- | 左剰余類での分解
leftExtractBy :: [Sym] -> [Sym] -> [[Sym]]
xs `leftExtractBy` ys = map (fst <$>) $ groupBy ((==) `on` snd) $ sortOn snd xs'
  where
    xs' :: [(Sym, Set.Set Sym)]
    xs' = zipWith (\x x' -> (x, Set.fromList $ x' -*< ys)) xs xs

-- | 右剰余類での分解
rightExtractBy :: [Sym] -> [Sym] -> [[Sym]]
xs `rightExtractBy` ys = map (fst <$>) $ groupBy ((==) `on` snd) $ sortOn snd xs'
  where
    xs' :: [(Sym, Set.Set Sym)]
    xs' = zipWith (\x x' -> (x, Set.fromList $ ys >*- x')) xs xs


(-*<) :: Sym -> [Sym] -> [Sym]
x -*< xs = map (x `apply`) xs
(>*-) :: [Sym] -> Sym -> [Sym]
xs >*- x = map (`apply` x) xs
