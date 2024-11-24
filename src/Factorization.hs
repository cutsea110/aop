module Factorization where

import Data.List (unfoldr, find)

wrap :: a -> [a]
wrap = (:[])
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)
cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) (x, y) = (f x, g y)

merge :: Ord a => [a] -> [a] -> [a]
merge xxs@(x:xs) yys@(y:ys) = case compare x y of
  LT -> x:merge xs yys
  EQ -> x:merge xs ys
  GT -> y:merge xxs ys

xmerge :: Ord a => [a] -> [a] -> [a]
xmerge (x:xs) ys = x:merge xs ys

mergeAll :: Ord a => [[a]] -> [a]
mergeAll (xs:xss) = xmerge xs (mergeAll xss)

primes :: [Integer]
primes = 2 : ([3..] \\ composites)
  where
    composites = mergeAll [map (p*) [p..] | p <- primes]

infixl 9 \\
(\\) :: Ord a => [a] -> [a] -> [a]
xxs@(x:xs) \\ yys@(y:ys) = case compare x y of
  LT -> x : (xs \\ yys)
  EQ -> xs \\ ys
  GT -> xxs \\ ys

badFactorize :: Integer -> [Integer]
badFactorize = unfoldr facM

facM :: Integer -> Maybe (Integer, Integer)
facM n = fmap (\p -> (p, n `div` p)) d
  where
    d = find (\p -> n `mod` p == 0) ps
    ps = takeWhile (<= n') primes
    n' = floor $ sqrt $ fromIntegral n

unfoldnel :: (a -> Either b (b, a)) -> a -> [b]
unfoldnel f = u
  where u = either wrap (uncurry (:) . cross (id, u)) . f

factorize :: Integer -> [Integer]
factorize = unfoldnel facE

facE :: Integer -> Either Integer (Integer, Integer)
facE n = maybe (Left n) Right $ facM n
