module Group where

import Control.Arrow (second)
import Data.Function (on)
import Data.List (sortBy, sortOn, groupBy, inits)
import Data.Monoid (Endo(..), Sum(..))
import qualified Data.Set as Set
import Text.Printf (printf)

import BinaryIndexedTree (BIT, new, inc', (!))
import Combinatorial (perms)

-- | 次元
type Dimension = Int
-- | 対称群の元の置換表現
data Replace = Replace { dimension :: Dimension
                       , mapTo     :: [Int]
                       }
             deriving (Eq, Ord, Show)

instance Semigroup Replace where
  (<>) = compose

-- | 対称群の元の型
type TYP = [Int]
-- | 対称群の元の軌道表現
data Orbit = Orbit { typeOf :: TYP
                   , reprOf ::  [[Int]]
                   }
           deriving (Eq, Ord, Show)

syms :: Dimension -> [Replace]
syms n = Replace n <$> perms [1..n]

toEndo :: Replace -> Endo Replace
toEndo = Endo . compose

-- | 合成 (右から左へ合成する)
compose :: Replace -> Replace -> Replace
(Replace n2 xs2) `compose` (Replace n1 xs1)
  | n2 == n1   = Replace n2 ys2
  | otherwise = error "compose: dimensions do not match"
  where
    f = map snd . sortOn fst
    ys1 = f $ zip xs1 [1..n1]
    ys2 = f $ zip ys1 xs2

-- | 逆元
complement :: Replace -> Replace
complement (Replace n xs) = Replace n xs'
  where xs' = map fst . sortOn snd . zip [1..] $ xs

-- | f の g による共役元
covariantOver :: Replace -> Replace -> Replace
f `covariantOver` g = g `compose` f `compose` g'
  where g' = complement g

-- | g による自己同型写像
auto :: Replace -> Replace -> Replace
auto g = (`covariantOver` g) -- auto == flip covariantOver

-- | 3次対称群の正規部分群
g3 :: [Replace]
g3 = map (Replace 3) [[1,2,3],[2,3,1],[3,1,2]]

-- | 4次対称群の正規部分群
g4 :: [Replace]
g4 = map (Replace 4) [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]]

-- | 5次対称群の正規部分群
g5 :: [Replace]
g5 = map (Replace 5) [[1,2,3,4,5],[2,3,1,4,5],[3,1,2,4,5],[2,4,3,1,5]
                     ,[4,1,3,2,5],[3,2,4,1,5],[4,2,1,3,5],[1,3,4,2,5]
                     ,[1,4,2,3,5],[2,1,4,3,5],[3,4,1,2,5],[4,3,2,1,5]]

-- | 左剰余類での分解
leftExtractBy :: [Replace] -> [Replace] -> [[Replace]]
xs `leftExtractBy` ys = f xs'
  where
    f :: [(Replace, Set.Set Replace)] -> [[Replace]]
    f = map (fst <$>) . groupBy ((==) `on` snd) . sortOn snd
    xs' :: [(Replace, Set.Set Replace)]
    xs' = zipWith (\x x' -> (x, Set.fromList $ x' -*< ys)) xs xs

-- | 右剰余類での分解
rightExtractBy :: [Replace] -> [Replace] -> [[Replace]]
xs `rightExtractBy` ys = f xs'
  where
    f :: [(Replace, Set.Set Replace)] -> [[Replace]]
    f = map (fst <$>) . groupBy ((==) `on` snd) . sortOn snd
    xs' :: [(Replace, Set.Set Replace)]
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
        | v == s    -> (reverse (found:acc), xs')
        | otherwise -> go (found:acc) v xs'

choice :: Eq a => a -> [(a, b)] -> Maybe ((a, b), [(a, b)])
choice k = go []
  where
    go acc [] = Nothing
    go acc (x@(k', v):xs)
      | k == k'   = Just (x, reverse acc ++ xs)
      | otherwise = go (x:acc) xs

toOrbit :: Replace -> Orbit
toOrbit (Replace n xs) = let xs' = f xs in Orbit (map length xs') xs'
  where f = map (fst <$>) . sortBy (compareDesc `on` length) . walk . zip [1..n]
        compareDesc = flip compare

fromOrbit :: Orbit -> Replace
fromOrbit = Replace <$> sum . typeOf <*> f . reprOf
  where
    f :: [[Int]] -> [Int]
    f = map snd . sortOn fst . concatMap fromCycle
    fromCycle :: [a] -> [(a, a)]
    fromCycle = zip <$> id <*> tail . cycle

toSimpleOrbit :: Replace -> [[Int]]
toSimpleOrbit = filter pred . reprOf . toOrbit
  where pred = (>1) . length

(-*<) :: Replace -> [Replace] -> [Replace]
x -*< xs = map (x `compose`) xs
(>*-) :: [Replace] -> Replace -> [Replace]
xs >*- x = map (`compose` x) xs


-- | 隣接互換(elementary transposition)
elemTrans :: [Int] -> [(Int,Int)]
elemTrans xs = concatMap (map f. reverse) zs
  where f i = (i, i+1)
        ys = zipWith (\i s -> i-(s+1)) xs (countSmallL xs)
        zs = zipWith (\l s -> [s..s+l-1]) ys [1..]


-- リストの各要素より小さい要素が左側にいくつあるか数え上げる
countSmallL :: [Int] -> [Int]
countSmallL xs = map (snd . second getSum) $ go xs b []
  where n = maximum xs
        b = new n :: BIT (Sum Int)
        go []     b acc = zip xs (reverse acc)
        go (x:xs) b acc = let acc' = b ! x : acc -- x 登場時点で、x より小さい要素の出現を数える
                              b' = inc' x 1 b
                          in go xs b' acc'


------
-- | あみだくじを AA で描画する
drawAmida :: [Int] -> IO ()
drawAmida xs = do
  let q = show xs
  putStrLn q
  putStrLn $ replicate (length q) '-'
  putStrLn $ showNums n
  mapM_ (putStrLn . offset . showRow n . fst) $ reverse $ elemTrans xs
  putStrLn $ showNums n
  putStr "\n"
  where n = length xs
        offset = ("   "++)

-- | あみだくじの一行を AA で描画する
showRow :: Int -> Int -> String
showRow n i = concatMap f [1..n]
  where f j | j == n    = "|"
            | j == i    = "|---"
            | otherwise = "|   "

-- | あみだくじのヘッダ/フッタを描画する
showNums :: Int -> String
showNums n = concatMap (printf "%4d") [1..n]
