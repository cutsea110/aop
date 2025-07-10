{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module RLE where

import Data.List (unfoldr)

toRLE :: Eq a => [a] -> [(a, Int)]
toRLE = unfoldr psi
  where
    psi [] = Nothing
    psi xxs@(x:xs) = Just ((x, length ys), zs)
      where (ys, zs) = span (== x) xxs
