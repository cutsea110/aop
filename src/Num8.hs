-- | 0 から 9 までの 10 種類の数字を高々一度だけ使って 4 桁の数字を 2 つ用意する
-- それら 2 つの数の差の最小を求めよ
--
module Num8 where

import Data.List (foldl')
import Control.Arrow ((***))
import Control.Monad.State

select :: [Int] -> [(Int, [Int])]
select [x] = [(x, [])]
select (x:xs) = (x,xs) : [ (y, x:ys)
                         | (y, ys) <- select xs
                         ]

perms :: Int -> [Int] -> [([Int], [Int])]
perms 0 xs = [([], xs)]
perms n xs = [ (y:zs, ws)
             | (y, ys) <- select xs
             , (zs, ws) <- perms (n-1) ys
             ]

gen :: Int -> [([Int], [Int])]
gen n = [ (xs, zs)
        | (xs, ys) <- perms n [0..9]
        , (zs, ws) <- perms n ys
        ]

gen' :: Int -> [(Int, Int)]
gen' = filter (uncurry (<)) . map (toInt***toInt ) . gen
  where
    toInt = foldl' (\b a -> b*10+a) 0

num8 :: StateT (Int, Int, Int) IO ()
num8 = do
  forM_ (gen' 4) $ \(xs, ys) -> do
    let v' = (xs, ys, ys-xs)
    v <- get
    when (thd3 v' < thd3 v) $ do
      { put v'
      ; liftIO $ putStrLn $ "=> " ++ show v'
      }
  where
    thd3 (_, _, x) = x


main :: IO (Int, Int, Int)
main = evalStateT (num8 >> get) (9999, 0, 9999)
