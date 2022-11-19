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

swapCons :: Ord a => (a, [a]) -> [a]
swapCons (x, [])     = [x]
swapCons (x, (y:ys)) | x <= y    = x:y:ys
                     | otherwise = y:x:ys

bubbleSort :: [Int] -> [Int]
bubbleSort = ana (out . (cata ([], swapCons) $?))

out :: [a] -> Maybe (a, [a])
out []     = Nothing
out (x:xs) = Just (x, xs)

-----
swapUncons :: Ord a => [a] -> Maybe (a, [a])
swapUncons [] = Nothing
swapUncons (x:[]) = Just (x, [])
swapUncons (x:y:ys) | x <= y    = Just (x, y:ys)
                    | otherwise = Just (y, x:ys)

naiveInsertSort :: [Int] -> [Int]
naiveInsertSort = cata ([], (ana swapUncons $?) . in')

in' :: (a, [a]) -> [a]
in' (x, xs) = x:xs

-------------------------------

--
--           [nil, cons]
--    [a] <------------- 1 + a * [a]
--     |                   |
--  u  |                   |  id + id_a * (id * u)
--     v                   v
--     X  <------------- 1 + a * ([a] * X)
--           [d, g]
--
para :: (b, (a, ([a], b)) -> b) -> [a] -> b
para (d, g) = u
  where u [] = d
        u (x:xs) = g (x, (xs, u xs))

dropWhile' :: (Int -> Bool) -> [Int] -> [Int]
dropWhile' p = para ([], phi)
  where phi (x, (xs, b)) = if p x then b else x:xs

--
--           out
--    [a] -------------> 1 + a * [a]
--     ^                   ^
--  v  |                   |  id + id_a * (id + v)
--     |                   |
--     X  -------------> 1 + a * ([a] + X)
--           psi
--
apo :: (b -> Maybe (a, (Either [a] b))) -> b -> [a]
apo psi = v
  where v xs = case psi xs of
          Nothing -> []
          Just (a, Left as) -> a:as
          Just (a, Right b) -> a:v b

tails' :: [Int] -> [[Int]]
tails' = apo psi
  where psi []       = Just ([], Left [])
        psi s@(_:xs) = Just (s, Right xs)

swapUncons' :: [Int] -> Maybe (Int, Either [Int] [Int])
swapUncons' []                   = Nothing
swapUncons' (x:[])               = Just (x, Left [])
swapUncons' (x:y:ys) | x <= y    = Just (x, Left  (y:ys))
                     | otherwise = Just (y, Right (x:ys))

insertSort :: [Int] -> [Int]
insertSort = cata ([], (apo swapUncons' $?) . in')

select :: (Int, ([Int], [Int])) -> [Int]
select (x, (xs,  [])) = x:xs
select (x, (xs, y:ys)) | x <= y    = x:xs
                       | otherwise = y:x:ys

selectSort :: [Int] -> [Int]
selectSort = ana (out . (para ([], select) $?))
