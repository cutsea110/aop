module SimpleSort where

import Debug.Trace (trace)

f $? x = let v = f x in trace ("{- " ++ show x ++ " => " ++ show v ++ " -}") v

test :: [Int]
test = [3, 7, 1, 9, 6, 2, 5, 8, 4]

--
--           [nil, cons]
--    [a] <------------- 1 + a * [a]
--     |                   |
--  u  |                   |  id + id_a * u
--     v                   v
--     X  <------------- 1 + a *  X
--           [c, f]
--

cata :: (b, (a, b) -> b) -> [a] -> b
cata (c, f) = u
  where u []     = c
        u (x:xs) = f (x, u xs)

--
--             out
--    [a] -------------> 1 + a * [a]
--     ^                   ^
--  v  |                   |  id + id_a * v
--     |                   |
--     X  -------------> 1 + a *  X
--            psi
--
ana :: (b -> Maybe (a, b)) -> b -> [a]
ana psi = v
  where v x = case psi x of
          Nothing     -> []
          Just (a, b) -> a:v b

bubble :: [Int] -> [Int]
bubble = cata ([], swapCons)
  where swapCons (x, [])     = [x]
        swapCons (x, (y:ys)) | x <= y    = x:y:ys
                             | otherwise = y:x:ys

bubbleSort :: [Int] -> [Int]
bubbleSort = ana psi
  where psi = out . (bubble $?)

out :: [a] -> Maybe (a, [a])
out []     = Nothing
out (x:xs) = Just (x, xs)

-----

insert :: [Int] -> [Int]
insert = ana psi
  where psi [] = Nothing
        psi (x:[]) = Just (x, [])
        psi (x:y:ys) | x <= y    = Just (x, y:ys)
                     | otherwise = Just (y, x:ys)

insertSort :: [Int] -> [Int]
insertSort = cata ([], (insert $?) . in')

in' :: (a, [a]) -> [a]
in' (x, xs) = x:xs
