module TakeWhile where

import Prelude hiding (takeWhile)
import Control.Monad (join)
import Data.List (unfoldr)

snoc :: [a] -> Maybe (a, [a])
snoc []     = Nothing
snoc (x:xs) = Just (x, xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = unfoldr phi
  where
    phi = join . fmap f . snoc
      where
        f (x, xs) | p x       = Just (x, xs)
                  | otherwise = Nothing

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr f c
  where
    c = []
    f x xs | p x       = x:xs
           | otherwise = []
