-- | 0 から 9 までの 10 種類の数字を高々一度だけ使って 4 桁の数字を 2 つ用意する
-- それら 2 つの数の差の最小を求めよ
--
module Num8 where

import Data.List ((\\), foldl')
import Control.Monad.State

select :: [Int] -> [(Int, [Int])]
select [x] = [(x, [])]
select (x:xs) = (x,xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

perms :: Int -> [Int] -> [([Int], [Int])]
perms 0 [] = [([], [])]
perms 0 xs = [([], xs)]
perms n xs = [(y:zs, ws) | (y, ys) <- select xs, (zs, ws) <- perms (n-1) ys]

gen :: Int -> [([Int], [Int])]
gen n = [(xs, zs) | (xs, ys) <- perms n [0..9], (zs, ws) <- perms n ys, xs < zs]

calc :: [Int] -> [Int] -> (Int, Int, Int)
calc xs ys = (xs', ys', ys'-xs')
  where
    num i j = i*10+j
    tapply f (x, y) = (f x, f y)
    (xs', ys') = tapply (foldl' num 0) (xs, ys)

num8 :: StateT (Int, Int, Int) IO ()
num8 = forM_ (gen 4) $ \(xs, ys) -> do
  let upd@(_, _, v') = calc xs ys
  (_, _, v) <- get
  when (v' < v) $ do
    { put upd
    ; liftIO $ putStrLn $ "=> " ++ show upd
    }

main :: IO (Int, Int, Int)
main = evalStateT (num8 >> get) (9999, 0, 9999)
