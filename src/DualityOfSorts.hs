module DualityOfSorts where
-- ref.) Duality of Sorts by Rafl Hinze

insertSort :: [Integer] -> [Integer]
insertSort = foldr insert []

insert :: Integer -> [Integer] -> [Integer]
insert y ys = xs ++ [y] ++ zs
  where (xs, zs) = span (<=y) ys
