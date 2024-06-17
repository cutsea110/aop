module BoringScore where

import Data.List (unfoldr, mapAccumL)

type Throw = Int
type Bonus = Int
type Score = Int

data Frame = Strike [Bonus]
           | Spare [Throw] [Bonus]
           | Pair [Throw]
           deriving (Show)

points :: Frame -> Score
points (Strike   bs) = 10 + sum bs
points (Spare _  bs) = 10 + sum bs
points (Pair  ts   ) = sum ts

frames :: [Throw] -> [Frame]
frames ts = unfoldr psi ts
  where
    psi :: [Throw] -> Maybe (Frame, [Throw])
    psi [] = Nothing
    psi (10:ts) = Just (Strike (take 2 ts), ts)
    psi (x:y:ts)
      | x + y == 10 = Just (Spare [x, y] (take 1 ts), ts)
      | otherwise   = Just (Pair [x, y], ts)
    psi ts = Just (Pair t, d) where (t, d) = splitAt 2 ts

scores :: [Frame] -> [Score]
scores = snd . mapAccumL psi 0
  where
    psi ttl f = let ttl' = ttl + points f in dup ttl'
    dup x = (x, x)

game :: [Throw] -> [(Frame, Score)]
game = take 10 . (zip <$> id <*> scores) . frames

-------------------------

test:: [Throw]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]

perfect:: [Throw]
perfect = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
