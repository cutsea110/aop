module SimpleSort where

import Debug.Trace (trace)

f $? x = let v = f x in trace (" {- " ++ show x ++ " => " ++ show v ++ " -}") v


test :: [Int]
test = [3,7,5,1,8,2,6,9,4]

-- bubble
-- sorted            sorting
--                  [3,7,5,1,8,2,6,9,4]
--                                 ^ ^
--                  [3,7,5,1,8,2,6,4,9]
--                               ^ ^
--                  [3,7,5,1,8,2,4,6,9]
--                             ^ ^
--                  [3,7,5,1,8,2,4,6,9]
--                           ^ ^
--                  [3,7,5,1,2,8,4,6,9]
--                         ^ ^
--                  [3,7,5,1,2,8,4,6,9]
--                       ^ ^
--                  [3,7,1,5,2,8,4,6,9]
--                     ^ ^
--                  [3,1,7,5,2,8,4,6,9]
--                   ^ ^
--                  [1,3,7,5,2,8,4,6,9]
--
-- 1,               [3,7,5,2,8,4,6,9]
--                   :
-- 1,2,             [3,7,5,4,8,6,9]

--------------------
--             [nil, cons]
--     [a] <--------------- 1 + a * [a]
--      |                     |
--   u  |                     | id + id_a * u
--      V                     V
--      b  <--------------- 1 + a *  b
--             [c, f]
cata :: (b, (a, b) -> b) -> [a] -> b
cata (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

-- [3,2,1]
--    ||
-- 3:(2:(1:[]))
-- 3 `f` (2 `f` (1 `f` c))

--             out
--     [a] ---------------> 1 + a * [a]
--      A                     A
--   v  |                     | id + id_a * v
--      |                     |
--      b  ---------------> 1 + a *  b
--             psi
ana :: (b -> Maybe (a, b)) -> b -> [a]
ana psi = v
  where v x = case psi x of
          Nothing     -> []
          Just (a, b) -> a:v b


--             [nil, cons]
--     [a] <--------------- 1 + a * [a]
--      |                     |
--   u  |                     | id + id_a * (id * u)
--      V                     V
--      b  <--------------- 1 + a * ([a] * b)
--             [d, g]
para :: (b, (a, ([a], b)) -> b) -> [a] -> b
para (d, g) = u
  where u [] = d
        u (x:xs) = g (x, (xs, u xs))

--             out
--     [a] ---------------> 1 + a * [a]
--      A                     A
--   v  |                     | id + id_a * (id + v)
--      |                     |
--      b  ---------------> 1 + a * ([a] + b)
--             psi
apo :: (b -> Maybe (a, Either [a] b)) -> b -> [a]
apo psi = v
  where v b = case psi b of
          Nothing -> []
          Just (x, Left  xs) -> x:xs
          Just (x, Right xs) -> x:v xs

------------------------

bubble = cata ([], swapCons)

swapCons :: (Int, [Int]) -> [Int]
swapCons (x, [])   = [x]
swapCons (x, y:ys) | x <= y    = x:y:ys
                   | otherwise = y:x:ys

bubbleSort = ana (out . (bubble $?))

out :: [a] -> Maybe (a, [a])
out []     = Nothing
out (x:xs) = Just (x, xs)

------------------------

-- insert
-- sorting              sorted
-- [3,7,5,1,8,2,6,9,4]  []
-- [3,7,5,1,8,2,6,9]    [4]
-- [3,7,5,1,8,2,6]      [9,4]
-- [3,7,5,1,8,2,6]      [4,9]
-- [3,7,5,1,8,2]        [4,6,9]
-- [3,7,5,1,8]          [2,4,6,9]

-- [3,1,2,4]
-- 3:1:2:4:[]
-- 1,  3:2:4:[]
-- 1,2, 3:4:[]
-- 1,2,3, 4:[]
-- 1,2,3,4, []
insert = ana swapUncons

swapUncons :: [Int] -> Maybe (Int, [Int])
swapUncons [] = Nothing
swapUncons (x:[]) = Just (x, [])
swapUncons (x:y:ys) | x <= y    = Just (x, y:ys)
                    | otherwise = Just (y, x:ys)

insertSort = cata ([], (insert $?) . in')

in' :: (a, [a]) -> [a]
in' (x, xs) = x:xs

------------------------

insert' = apo (swapUncons' $?)

swapUncons' :: [Int] -> Maybe (Int, Either [Int] [Int])
swapUncons' [] = Nothing
swapUncons' (x:[]) = Just (x, Left [])
swapUncons' (x:y:ys) | x <= y    = Just (x, Left  (y:ys))
                     | otherwise = Just (y, Right (x:ys))

insertSort' = cata ([], (insert' $?) . in')

------------------------

-- [3,2,1]
-- [3,1,2]
-- [1,3,2]
-- 1 [3,2]

-- [0,3,2,1]
-- [0,3,1,2]
-- [0,1,3,2]
-- 0 [3,2,1] --[1,3,2]
select = para ([], swapCons')

swapCons' :: (Int, ([Int], [Int])) -> [Int]
swapCons' (x, (_, []))   = [x]
swapCons' (x, (xs, y:ys)) | x <= y    = x:xs
                          | otherwise = y:x:ys

selectSort = ana (out . (select $?))
