module DualityOfSorts where
-- ref.) Duality of Sorts by Rafl Hinze

import Data.List (delete, unfoldr)

insertSort :: [Integer] -> [Integer]
insertSort = foldr insert []

insert :: Integer -> [Integer] -> [Integer]
insert y ys = xs ++ [y] ++ zs
  where (xs, zs) = span (<=y) ys

selectSort :: [Integer] -> [Integer]
selectSort = unfoldr select

select :: [Integer] -> Maybe (Integer, [Integer])
select [] = Nothing
select xs = Just (x, xs')
  where x   = minimum xs
        xs' = delete x xs
