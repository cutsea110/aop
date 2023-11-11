module Group where

import Data.Function (on)
import Data.List (sortBy, sortOn, groupBy)
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

simpleRepr :: Sym -> [[Int]]
simpleRepr s = filter (\xs -> length xs > 1) $ repr s

typeOf :: Sym -> [Int]
typeOf = map length . repr

repr :: Sym -> [[Int]]
repr (Sym n xs) = f xs
  where f = map (fst <$>) . sortBy (compareDesc `on` length) . follows . zip [1..n]
        compareDesc = flip compare

follows :: (Eq a, Show a) =>  [(a, a)] -> [[(a, a)]]
follows [] = []
follows xs@((k,v):_) = found : follows rest
  where (found, rest) = follow k xs

follow :: (Eq a, Show a) => a -> [(a, a)] -> ([(a, a)], [(a, a)])
follow s xs = go [] s xs
  where
    go acc n xs = case choice n xs of
      Nothing -> error $ "follow failed: " ++ show n ++ " not found in keys of " ++ show xs
      Just (found@(_, v), xs') ->
        if s == v
        then (reverse (found:acc), xs')
        else go (found:acc) v xs'

choice :: Eq a => a -> [(a, b)] -> Maybe ((a, b), [(a, b)])
choice k = go []
  where
    go acc [] = Nothing
    go acc (x@(k', v):xs)
      | k == k'   = Just (x, reverse acc ++ xs)
      | otherwise = go (x:acc) xs

fromRepr :: Ord a => [[a]] -> [a]
fromRepr = map snd . sortOn fst . concatMap fromCycle

fromCycle :: [a] -> [(a, a)]
fromCycle xs = zip xs (tail xs ++ [head xs])

(-*<) :: Sym -> [Sym] -> [Sym]
x -*< xs = map (x `apply`) xs
(>*-) :: [Sym] -> Sym -> [Sym]
xs >*- x = map (`apply` x) xs
