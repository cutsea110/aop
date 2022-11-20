module SimpleSort where

import Debug.Trace (trace)

f $? x = let v = f x in trace (" {- " ++ show x ++ " => " ++ show v ++ " -}") v


test :: [Int]
test = [3,7,1,5,6,8,4,9,2]

-- sorted     sorting
-- []  [3,7,1,5,6,8,4,9,2]
-- []                 ^ ^
-- []  [3,7,1,5,6,8,4,2,9]
-- []               ^ ^
-- []  [3,7,1,5,6,8,2,4,9]
-- []             ^ ^
-- []  [3,7,1,5,6,2,8,4,9]
-- []           ^ ^
-- []  [3,7,1,5,2,6,8,4,9]
-- []         ^ ^
-- []  [3,7,1,2,5,6,8,4,9]
-- []       ^ ^
-- []  [3,7,1,2,5,6,8,4,9]
-- []     ^ ^
-- []  [3,1,7,2,5,6,8,4,9]
-- []   ^ ^
-- []  [1,3,7,2,5,6,8,4,9]
--
-- [1] [3,7,2,5,6,8,4,9]

--             [nil, cons]
--    [a] <------------------- 1 + a * [a]
--     |                         |
--   u |                         | id + id_a * u
--     V                         V
--     b  <------------------- 1 + a *  b
--              [c, f]
cata :: (b, (a, b) -> b) -> [a] -> b
cata (c, f) = u
  where u []     = c
        u (x:xs) = f (x, u xs)

--              out
--    [a] -------------------> 1 + a * [a]
--     A                         A
--   v |                         | id + id_a * v
--     |                         |
--     b  -------------------> 1 + a *  b
--              psi
ana :: (b -> Maybe (a, b)) -> b -> [a]
ana psi = v
  where v x = case psi x of
          Nothing     -> []
          Just (a, b) -> a:v b


--             [nil, cons]
--    [a] <------------------- 1 + a * [a]
--     |                         |
--   u |                         | id + id_a * (id * u)
--     V                         V
--     b  <------------------- 1 + a *  ([a] * b)
--              [d, g]
para :: (b, (a, ([a], b)) -> b) -> [a] -> b
para (d, g) = u
  where u [] = d
        u (x:xs) = g (x, (xs, u xs))


dropWhile' p = para ([], g)
  where g (x, (xs, ys)) = if p x then ys else x:xs


--              out
--    [a] -------------------> 1 + a * [a]
--     A                         A
--   v |                         | id + id_a * (id + v)
--     |                         |
--     b  -------------------> 1 + a *  ([a] + b)
--              psi
apo :: (b -> Maybe (a, Either [a] b)) -> b -> [a]
apo psi = v
  where v b = case psi b of
          Nothing -> []
          Just (x, Left  xs) -> x:xs
          Just (x, Right xs) -> x:v xs

tails' = apo psi
  where psi []     = Just ([], Left [])
        psi (x:xs) = Just (x:xs, Right xs)

---------------------------------------------------------------------
--  [2,3,1]
--  2 : (3 : (1 : []))
--  2 `f` (3 `f` (1 `f` c))
bubble = cata ([], swapCons)

swapCons :: (Int, [Int]) -> [Int]
swapCons (x, []) = [x]
swapCons (x, y:ys) | x <= y    = x:y:ys
                   | otherwise = y:x:ys

bubbleSort = ana (out . (bubble $?))

out :: [a] -> Maybe (a, [a])
out []     = Nothing
out (x:xs) = Just (x, xs)

-- sorting             sorted
-- [3,7,1,5,6,8,4,9,2]  []
-- [3,7,1,5,6,8,4,9]  [2]
-- [3,7,1,5,6,8,4]  [9,2]
-- [3,7,1,5,6,8,4]  [2,9]
-- [3,7,1,5,6,8]  [4,2,9]
-- [3,7,1,5,6,8]  [2,4,9]
-- [3,7,1,5,6]  [8,2,4,9]
-- [3,7,1,5,6]  [2,4,8,9]
-- [3,7,1,5]  [2,4,6,8,9]
-- :

-- 8:2:4:9:[]
-- 2, 4, 8, 9, []
insert = ana swapUncons

swapUncons :: [Int] -> Maybe (Int, [Int])
swapUncons [] = Nothing
swapUncons (x:[]) = Just (x, [])
swapUncons (x:y:ys) | x <= y    = Just (x, y:ys)
                    | otherwise = Just (y, x:ys)

insertSort = cata ([], (insert $?) . in')

in' :: (a, [a]) -> [a]
in' (x, xs) = x:xs


insert' = apo (swapUncons' $?)

swapUncons' :: [Int] -> Maybe (Int, Either [Int] [Int])
swapUncons' [] = Nothing
swapUncons' (x:[]) = Just (x, Left [])
swapUncons' (x:y:ys) | x <= y    = Just (x, Left (y:ys))
                     | otherwise = Just (y, Right (x:ys))

insertSort' = cata ([], insert' . in')


select = para ([], (swapCons' $?))

swapCons' :: (Int, ([Int], [Int])) -> [Int]
swapCons' (x, (_, [])) = [x]
swapCons' (x, (xs, y:ys)) | x <= y    = x:xs
                          | otherwise = y:x:ys

selectSort = ana (out . select)
