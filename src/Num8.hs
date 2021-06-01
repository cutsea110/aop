-- | 0 から 9 までの 10 種類の数字を高々一度だけ使って 4 桁の数字を 2 つ用意する
-- それら 2 つの数の差の最小を求めよ
--
module Num8 where

import Data.List (foldl')
import Control.Arrow ((***))
import Control.Monad.State (StateT, evalStateT, get, put, forM_, when, liftIO)

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

num :: Int -> StateT (Int, Int, Int) IO ()
num n = do
  forM_ (gen' n) $ \(xs, ys) -> do
    let v' = (xs, ys, ys-xs)
    v <- get
    when (thd3 v' < thd3 v) $ do
      { put v'
      ; liftIO $ putStrLn $ "=> " ++ show v'
      }

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

quiz :: Int -> IO Int
quiz n | n <= 5  = do { num n
                      ; (_, _, v) <- get
                      ; return v
                      } `evalStateT` (def, 0, def)
       | otherwise = fail "digits must be below 5"
  where
    def = 10^n-1

main :: IO Int
main = quiz 4
