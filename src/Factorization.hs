module Factorization where

import Data.List (unfoldr, find)

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
unfoldnel f x = case f x of
  Left y -> [y]
  Right (y, z) -> y : unfoldnel f z

factorize :: Integer -> [Integer]
factorize = unfoldnel facE

facE :: Integer -> Either Integer (Integer, Integer)
facE n = case facM n of
  Nothing -> Left n
  Just (p, q) -> Right (p, q)
