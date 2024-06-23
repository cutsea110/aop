module BowlingScore where

import Data.List (transpose, unfoldr, mapAccumL, zipWith4)

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
state f | onGoing f = Pending
        | otherwise = Fixed
  where
    onGoing :: Frame -> Bool
    onGoing (Strike  bs) = length bs < 2
    onGoing (Spare _ bs) = null bs
    onGoing (Pair    ts) = length ts < 2

states :: [Frame] -> [FrameState]
states = map state

data Frame' = Frame' { getIdx   :: FrameIdx
                     , getFrame :: Frame
                     , getScore :: Score
                     , getState :: FrameState
                     } deriving (Show)

type Game = [Frame']

game :: [Throw] -> Game
game = take 10 . (zipWith4 Frame' <$> const [1..] <*> id <*> scores <*> states) . frames

type FrameIdx = Int

showFrame :: Frame' -> [String]
showFrame (Frame' i f s _) = case i of
  10 -> [ "+-----"
        , "|  10 "
        , "+-+-+-"
        , drawPoint f
        , "| +-+-"
        , drawScore s
        , "+-----"
        ]
    where
      drawPoint :: Frame -> String
      drawPoint (Strike      [b1,b2]) = "|X|" ++ show' b1 ++ "|" ++ show' b2
       where show' 10 = "X"
             show' n  = show n
      drawPoint (Spare [x,_] [b]) = "|" ++ show x ++ "|/|" ++ show b
      drawPoint (Spare [x,_] []) = "|" ++ show x ++ "|/| "
      drawPoint (Pair  [x, y])  = "|" ++ show x ++ "|" ++ show y ++ "| "
      drawPoint (Pair  [x])     = "|" ++ show x ++ "| | "
  
      drawScore :: Score -> String
      drawScore s = "|" ++ pad ++ scoreStr
        where scoreStr = show s
              len = length scoreStr
              pad = replicate (5 - len) ' '

  _ -> [ "+---"
       , "| " ++ show i ++ " "
       , "+-+-"
       , drawPoint f
       , "| +-"
       , drawScore s
       , "+---"
       ]
    where
      drawPoint :: Frame -> String
      drawPoint (Strike      _) = "| |X"
      drawPoint (Spare [x,_] _) = "|" ++ show x ++ "|/"
      drawPoint (Pair  [x, y])  = "|" ++ show x ++ "|" ++ show y
      drawPoint (Pair  [x])     = "|" ++ show x ++ "| "

      drawScore :: Score -> String
      drawScore s = "|" ++ pad ++ scoreStr
        where scoreStr = show s
              len = length scoreStr
              pad = replicate (3 - len) ' '

showGame :: Game -> [String]
showGame = foldr phi close
  where
    phi :: Frame' -> [String] -> [String]
    phi f = zipWith (++) (showFrame f)
    close = [ "+"
            , "|"
            , "+"
            , "|"
            , "+"
            , "|"
            , "+"
            ]

drawGame :: Game -> IO ()
drawGame = putStr . unlines . showGame

-- | Test data

test:: [Throw]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]

perfect:: [Throw]
perfect = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
