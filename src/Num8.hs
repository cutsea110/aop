-- | 0 から 9 までの 10 種類の数字を高々一度だけ使って 4 桁の数字を 2 つ用意する
-- それら 2 つの数の差の最小を求めよ
--
module Num8 where

import Data.List ((\\), foldl')
import Control.Monad.State

select :: [Int] -> [(Int, [Int])]
select [x] = [(x, [])]
select (x:xs) = (x,xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

perms :: Int -> [Int] -> [[Int]]
perms 0 _ = [[]]
perms n xs = [y:zs | (y, ys) <- select xs, zs <- perms (n-1) ys]

cpp (x, y) = [(a, b) | a <- x, b <- y]

gen n = [ (xs, ys) | xs <- perms n [0..9], ys <- perms n ([0..9] \\ xs)]

calc :: [Int] -> [Int] -> (Int, Int, Int)
calc xs ys = (xs', ys', abs(xs'-ys'))
  where
    num i j = i*10+j
    xs' = foldl' num 0 xs
    ys' = foldl' num 0 ys

num8 :: StateT (Int, Int, Int) IO ()
num8 = forM_ (gen 4) $ \(xs, ys) -> do
  let upd@(xs', ys', v') = calc xs ys
  (_, _, v) <- get
  when (v' < v) $ do
    { put upd
    ; liftIO $ print upd
    }

main = evalStateT (num8 >> get) (9999, 0, 9999)
