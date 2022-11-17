module SimpleSort where

import Debug.Trace (trace)
import Data.List (unfoldr)

($?) :: Show a => (a -> a) -> a -> a
f $? x = let v = f x in trace ("{-" ++ show x ++ " => " ++ show v ++ " -}") v

-- bubble sort
-- [] [3 7 1 9 6 2 5 8 4]
-- [] [3 7 1 9 6 2 5 4 8]
-- [] [3 7 1 9 6 2 4 5 8]
-- [] [3 7 1 9 6 2 4 5 8]
-- [] [3 7 1 9 2 6 4 5 8]
-- [] [3 7 1 2 9 6 4 5 8]
-- [] [3 7 1 2 9 6 4 5 8]
-- [] [3 1 7 2 9 6 4 5 8]
-- [] [1 3 7 2 9 6 4 5 8]
-- [1] [3 7 2 9 6 4 5 8]

-- 3:7:1:9:6:2:5:8:4:[]
-- 3:7:1:9:6:2:5:8:4:[]
-- 3:7:1:9:6:2:5:8:4:[]
-- 3:7:1:9:6:2:5:4:8:[]

--          [nil, cons]
--    [a] <------------ 1 + a * [a]
--     |                   |
--  u  |                   |  id + id_a * u
--     v                   v
--     x  <------------ 1 + a *  x
--           [c, f]

test :: [Integer]
test = [3, 7, 1, 9, 6, 2, 5, 8, 4]

bubble :: [Integer] -> [Integer]
bubble = foldr f []
  where f x [] = [x]
        f x (y:ys) | x <= y    = x:y:ys
                   | otherwise = y:x:ys

bubbleSort :: [Integer] -> [Integer]
bubbleSort = unfoldr psi
  where psi xs = case bubble $? xs of
          [] -> Nothing
          (x:xs) -> Just (x, xs)
