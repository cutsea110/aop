module BoringScore where

import Data.List (foldl', unfoldr)

type Throw = Int
type Bonus = Int
type Score = Int

data Frame = Strike [Bonus]
           | Spare [Throw] [Bonus]
           | Pair [Throw]
           deriving (Show)

frames :: [Throw] -> [Frame]
frames ts = unfoldr psi ts
  where
    psi :: [Throw] -> Maybe (Frame, [Throw])
    psi [] = Nothing
    psi (10:ts) = Just (Strike (take 2 ts), ts)
    psi (x:y:ts)
      | x + y == 10 = Just (Spare [x, y] (take 1 ts), ts)
      | otherwise   = Just (Pair [x, y], ts)
    psi ts = Just (Pair (take 2 ts), drop 2 ts)

scores :: [Frame] -> [Score]
scores = reverse . foldl' g []
  where
    g :: [Score] -> Frame -> [Score]
    g acc (Strike   bs) = (ttl acc + 10 + sum bs) : acc
    g acc (Spare ts bs) = (ttl acc + 10 + sum bs) : acc
    g acc (Pair  ts   ) = (ttl acc + sum ts) : acc

    ttl []    = 0
    ttl (x:_) = x

scoreBoard :: [Throw] -> [(Frame, Score)]
scoreBoard = take 10 . uncurry zip . pair id scores . frames
  where
    pair f g x = (f x, g x)

-------------------------

test:: [Throw]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]

perfect:: [Throw]
perfect = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
