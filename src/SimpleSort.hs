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
-- :
-- [1 2] [3 7 4 9 6 5 8]
-- :
-- [1 2 3] [4 7 5 9 6 8]

-- 3:7:1:9:6:2:5:8:4:[]
-- 3:7:1:9:6:2:5:8:4:[]
-- 3:7:1:9:6:2:5:8:4:[]
-- 3:7:1:9:6:2:5:4:8:[]

-- :+: <= swapCons
-- 3 :+: 7 :+: 1 :+: 9 :+: 6 :+: 2 :+: 5 :+: 8 :+: 4 :+: []

--          [nil, cons]
--    [a] <------------ 1 + a * [a]
--     |                   |
--  u  |                   |  id + id_a * u
--     v                   v
--     x  <------------ 1 + a * x
--           [c, f]

test :: [Integer]
test = [3, 7, 1, 9, 6, 2, 5, 8, 4]

bubble :: [Integer] -> [Integer]
bubble = foldr swapCons []
  where swapCons x [] = [x]
        swapCons x (y:ys) | x <= y    = x:y:ys
                          | otherwise = y:x:ys

bubbleSort :: [Integer] -> [Integer]
bubbleSort = unfoldr (out . (bubble $?))
{-
bubbleSort = unfoldr psi
  where psi xs = case bubble xs of
          [] -> Nothing
          (x:xs) -> Just (x, xs)
-}
out :: [Integer] -> Maybe (Integer, [Integer])
out []     = Nothing
out (x:xs) = Just (x, xs)

--------------------------------

-- naive insertion sort
-- [3 7 1 9 6 2 5 8 4] []
-- [3 7 1 9 6 2 5 8] [4]
-- [3 7 1 9 6 2 5] [8 4]
-- [3 7 1 9 6 2 5] [4 8]
-- [3 7 1 9 6 2] [5 4 8]
-- [3 7 1 9 6 2] [4 5 8]
-- [3 7 1 9 6 2] [4 5 8]

-- [5 3 2 4 8]
-- [3 5 2 4 8]
-- [3 2 5 4 8]
-- [3 2 4 5 8]
-- [3 2 4 5 8]

-- 5:3:2:4:8:[]
-- 3:5:2:4:8:[]
-- 3:2:5:4:8:[]
-- 3:2:4:5:8:[]
-- 3:2:4:5:8:[]

insert :: [Integer] -> [Integer]
insert = unfoldr swapUncons
  where swapUncons []    = Nothing
        swapUncons (x:[]) = Just (x, [])
        swapUncons (x:y:ys)
          | x <= y       = Just (x, y:ys)
          | otherwise    = Just (y, x:ys)

insertSort :: [Integer] -> [Integer]
insertSort = foldr f []
  where f x xs = insert $? (x:xs)
