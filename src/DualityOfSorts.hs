module DualityOfSorts where
-- ref.) Duality of Sorts by Rafl Hinze

import Debug.Trace (trace)
import Data.List (delete, unfoldr)

--------------------------------------------------------------------------------------
debug = True

tracer :: String -> a -> a
tracer = if debug then trace else const id

($?) :: (Show a, Show b) => (a -> b) -> a -> b
f $? x = let v = f x
             msg = " {- " ++ show x ++ " => " ++ show v ++ " -}"
         in tracer msg v
--------------------------------------------------------------------------------------

insertSort :: [Integer] -> [Integer]
insertSort = foldr (curry (insert $?)) []

insert :: (Integer, [Integer]) -> [Integer]
insert (y, ys) = xs ++ [y] ++ zs
  where (xs, zs) = span (<=y) ys

selectSort :: [Integer] -> [Integer]
selectSort = unfoldr (select $?)

select :: [Integer] -> Maybe (Integer, [Integer])
select [] = Nothing
select xs = Just (x, xs')
  where x   = minimum xs
        xs' = delete x xs
