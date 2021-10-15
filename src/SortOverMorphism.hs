module SortOverMorphism where
-- ref.) http://www.cs.ox.ac.uk/people/daniel.james/sorting/sorting.pdf
import Data.List (partition, unfoldr, delete)

insertSort :: [Integer] -> [Integer]
insertSort = foldr insert []

insert :: Integer -> [Integer] -> [Integer]
insert y ys = xs ++ [y] ++ zs
  where (xs, zs) = partition (<y) ys
  
selectSort :: [Integer] -> [Integer]
selectSort = unfoldr select

select :: [Integer] -> Maybe (Integer, [Integer])
select [] = Nothing
select xs = Just (x, xs')
  where x = minimum xs
        xs' = delete x xs
