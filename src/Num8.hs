-- | 0 から 9 までの 10 種類の数字を高々一度だけ使って 4 桁の数字を 2 つ用意する
-- それら 2 つの数の差の最小を求めよ
--
module Num8 where

import Data.List ((\\), foldl')
import Control.Monad.State

select :: [Int] -> [(Int, [Int])]
select [x] = [(x, [])]
select (x:xs) = (x,xs) : [ (y, x:ys)
                         | (y, ys) <- select xs
                         ]

perms :: Int -> [Int] -> [([Int], [Int])]
perms 0 [] = [([], [])]
perms 0 xs = [([], xs)]
perms n xs = [ (y:zs, ws)
             | (y, ys) <- select xs
             , (zs, ws) <- perms (n-1) ys
             ]

gen :: Int -> [([Int], [Int])]
gen n = [ (xs, zs)
        | (xs, ys) <- perms n [0..9]
        , (zs, ws) <- perms n ys
        , xs < zs -- 対称な組み合わせを排除
        ]

calc :: [Int] -> [Int] -> (Int, Int, Int)
calc xs ys = (xs', ys', ys'-xs')
  where
    tapply f (x, y) = (f x, f y)
    (xs', ys') = tapply (foldl' digit 0) (xs, ys)
    digit i j = i*10+j

num8 :: StateT (Int, Int, Int) IO ()
num8 = forM_ (gen 4) $ \(xs, ys) -> do
  let v' = calc xs ys
  v <- get
  when (thd3 v' < thd3 v) $ do
    { put v'
    ; liftIO $ putStrLn $ "=> " ++ show v'
    }

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x


main :: IO (Int, Int, Int)
main = evalStateT (num8 >> get) (9999, 0, 9999)
