module Group where

import Data.Function (on)
import Data.List (sortBy, sortOn, groupBy)
import Data.Monoid (Endo(..))
import qualified Data.Set as Set

import Combinatorial (perms)

-- | 次元
type DIM = Int
-- | 次元 n の対称群
data Sym = Sym DIM [Int] deriving (Eq, Ord, Show)

-- | 対称群の元の型
type TYP = [Int]
-- | 対称群の元の表現
data Repr = Repr TYP [[Int]] deriving (Eq, Ord, Show)

typeOf :: Repr -> TYP
typeOf (Repr t _) = t
reprOf :: Repr -> [[Int]]
reprOf (Repr _ xs) = xs

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

-- | 5次対称群の正規部分群
g5 :: [Sym]
g5 = map (Sym 5) [[1,2,3,4,5],[2,3,1,4,5],[3,1,2,4,5],[2,4,3,1,5]
                 ,[4,1,3,2,5],[3,2,4,1,5],[4,2,1,3,5],[1,3,4,2,5]
                 ,[1,4,2,3,5],[2,1,4,3,5],[3,4,1,2,5],[4,3,2,1,5]]

-- | 左剰余類での分解
leftExtractBy :: [Sym] -> [Sym] -> [[Sym]]
xs `leftExtractBy` ys = f xs'
  where
    f :: [(Sym, Set.Set Sym)] -> [[Sym]]
    f = map (fst <$>) . groupBy ((==) `on` snd) . sortOn snd
    xs' :: [(Sym, Set.Set Sym)]
    xs' = zipWith (\x x' -> (x, Set.fromList $ x' -*< ys)) xs xs

-- | 右剰余類での分解
rightExtractBy :: [Sym] -> [Sym] -> [[Sym]]
xs `rightExtractBy` ys = f xs'
  where
    f :: [(Sym, Set.Set Sym)] -> [[Sym]]
    f = map (fst <$>) . groupBy ((==) `on` snd) . sortOn snd
    xs' :: [(Sym, Set.Set Sym)]
    xs' = zipWith (\x x' -> (x, Set.fromList $ ys >*- x')) xs xs

walk :: (Eq a, Show a) =>  [(a, a)] -> [[(a, a)]]
walk [] = []
walk xs@((k,v):_) = found : walk rest
  where (found, rest) = step k xs

step :: (Eq a, Show a) => a -> [(a, a)] -> ([(a, a)], [(a, a)])
step s xs = go [] s xs
  where
    go acc n xs = case choice n xs of
      Nothing -> error $ "step failed: " ++ show n ++ " not found in keys of " ++ show xs
      Just (found@(_, v), xs')
        -> if s == v then (reverse (found:acc), xs') else go (found:acc) v xs'

choice :: Eq a => a -> [(a, b)] -> Maybe ((a, b), [(a, b)])
choice k = go []
  where
    go acc [] = Nothing
    go acc (x@(k', v):xs)
      | k == k'   = Just (x, reverse acc ++ xs)
      | otherwise = go (x:acc) xs

toRepr :: Sym -> Repr
toRepr (Sym n xs) = let xs' = f xs in Repr (map length xs') xs'
  where f = map (fst <$>) . sortBy (compareDesc `on` length) . walk . zip [1..n]
        compareDesc = flip compare

fromRepr :: Repr -> Sym
fromRepr = Sym <$> sum . typeOf <*> f . reprOf
  where
    f :: [[Int]] -> [Int]
    f = map snd . sortOn fst . concatMap fromCycle
    fromCycle :: [a] -> [(a, a)]
    fromCycle xs = zip xs (tail xs ++ [head xs])

(-*<) :: Sym -> [Sym] -> [Sym]
x -*< xs = map (x `apply`) xs
(>*-) :: [Sym] -> Sym -> [Sym]
xs >*- x = map (`apply` x) xs
