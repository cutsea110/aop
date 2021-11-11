module DualityOfSorts where
-- ref.) Duality of Sorts by Rafl Hinze

import Debug.Trace (trace)
import Data.List (delete, unfoldr)

--------------------------------------------------------------------------------------
debug = True

($?) :: Show a => (a -> b) -> a -> b
f $? x = if debug then trace (show x) (f x) else f x
--------------------------------------------------------------------------------------

insertSort :: [Integer] -> [Integer]
insertSort = foldr phi []
  where phi x xs = let v = insert (x, xs)
                       msg = " {- " ++ show (x, xs) ++ " => " ++ show v ++ " -}"
                   in trace msg v

insert :: (Integer, [Integer]) -> [Integer]
insert (y, ys) = xs ++ [y] ++ zs
  where (xs, zs) = span (<=y) ys

selectSort :: [Integer] -> [Integer]
selectSort = unfoldr psi
  where psi x = let v = select x
                    msg = " {- " ++ show x ++ " => " ++ show v ++ " -}"
                in trace msg v

select :: [Integer] -> Maybe (Integer, [Integer])
select [] = Nothing
select xs = Just (x, xs')
  where x   = minimum xs
        xs' = delete x xs
