module BowlingScore where

import Data.List (unfoldr, mapAccumL)

type Throw = Int
type Bonus = Int
type Score = Int

data Frame = Strike [Bonus]
           | Spare [Throw] [Bonus]
           | Pair [Throw]
           deriving (Show)

point :: Frame -> Score
point (Strike   bs) = 10 + sum bs
point (Spare _  bs) = 10 + sum bs
point (Pair  ts   ) = sum ts

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
    psi ttl f = let ttl' = ttl + point f in dup ttl'
    dup x = (x, x)

data FrameState = Pending | Fixed deriving (Show)

state :: Frame -> FrameState
state (Strike bs)
  | length bs < 2 = Pending
  | otherwise     = Fixed
state (Spare _ bs)
  | null bs   = Pending
  | otherwise = Fixed
state (Pair ts)
  | length ts < 2 = Pending
  | otherwise     = Fixed

states :: [Frame] -> [FrameState]
states = map state

data Frame' = Frame' { getFrame :: Frame
                     , getScore :: Score
                     , getState :: FrameState
                     } deriving (Show)

game :: [Throw] -> [Frame']
game = take 10 . (zipWith3 Frame' <$> id <*> scores <*> states) . frames

-------------------------

test:: [Throw]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]

perfect:: [Throw]
perfect = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
