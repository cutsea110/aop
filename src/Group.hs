module Group where

import Data.Array
import Data.Function (on)
import Data.List (sortBy, sortOn, groupBy, inits)
import Data.Monoid (Endo(..))
import qualified Data.Set as Set

import Combinatorial (perms)

-- | 次元
type DIM = Int
-- | 次元 n の対称群
data Sym = Sym DIM [Int] deriving (Eq, Ord, Show)

instance Semigroup Sym where
  (<>) = compose

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

toEndo :: Sym -> Endo Sym
toEndo = Endo . compose

-- | 合成 (右から左へ合成する)
compose :: Sym -> Sym -> Sym
(Sym n2 xs2) `compose` (Sym n1 xs1)
  | n2 == n1   = Sym n2 ys2
  | otherwise = error "compose: dimensions do not match"
  where
    ys1 = map fst $ sortOn snd $ zip [1..n1] xs1
    ys2 = map snd $ sortOn fst $ zip ys1 xs2

-- | 逆元
complement :: Sym -> Sym
complement (Sym n xs) = Sym n xs'
  where xs' = map fst . sortOn snd . zip [1..] $ xs

-- | f の g による共役元
covariantOver :: Sym -> Sym -> Sym
f `covariantOver` g = g `compose` f `compose` g'
  where g' = complement g

-- | g による自己同型写像
auto :: Sym -> Sym -> Sym
auto g = (`covariantOver` g) -- auto == flip covariantOver

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
    fromCycle = zip <$> id <*> tail . cycle

(-*<) :: Sym -> [Sym] -> [Sym]
x -*< xs = map (x `compose`) xs
(>*-) :: [Sym] -> Sym -> [Sym]
xs >*- x = map (`compose` x) xs


-- | 隣接互換(elementary transposition)
elemTrans :: [Int] -> [(Int,Int)]
elemTrans xs = concatMap (map f. reverse) zs
  where f i = (i, i+1)
        ys = zipWith (\i s -> i-(s+1)) xs (countSmallL xs)
        zs = zipWith (\l s -> [s..s+l-1]) ys [1..]

-- countSmallL :: [Int] -> [Int]
-- countSmallL xs = zipWith countSmall xs (inits xs)
--   where countSmall y = length . filter (y>)

-----

-- Binary Indexed Tree
data BIT = BIT Int (Array Int Int)

-- 初期化
initBIT :: Int -> BIT
initBIT n = BIT n (listArray (1, n) (replicate n 0))

-- 更新
update :: BIT -> Int -> Int -> BIT
update (BIT n arr) i val = BIT n (arr // [(i, (arr ! i) + val)])

-- 累積和
query :: BIT -> Int -> Int
query (BIT _ arr) i = sum $ map (arr !) [1..i]

-- リストの各要素より小さい要素が左側にいくつあるか数え上げる
countSmallL :: [Int] -> [Int]
countSmallL xs = reverse $ go (initBIT n) xs []
  where n = length xs
        go _   []     acc = acc
        go bit (x:xs) acc = go (update bit x 1) xs (query bit (x-1):acc)
