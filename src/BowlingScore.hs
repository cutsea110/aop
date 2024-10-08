module BowlingScore where

import Data.List (transpose, unfoldr, mapAccumL, zipWith4)

type Throw = Int
type Bonus = Int
type Score = Int

data Frame = Strike [Bonus]
           | Spare [Throw] [Bonus]
           | Open [Throw]
           deriving (Show)

point :: Frame -> Score
point (Strike   bs) = 10 + sum bs
point (Spare _  bs) = 10 + sum bs
point (Open  ts   ) = sum ts

frames :: [Throw] -> [Frame]
frames ts = unfoldr psi ts
  where
    psi :: [Throw] -> Maybe (Frame, [Throw])
    psi [] = Nothing
    psi (10:ts) = Just (Strike (take 2 ts), ts)
    psi (x:y:ts)
      | x + y == 10 = Just (Spare [x, y] (take 1 ts), ts)
      | otherwise   = Just (Open [x, y], ts)
    psi ts = Just (Open t, d) where (t, d) = splitAt 2 ts

scores :: [Frame] -> [Score]
scores fs = unfoldr psi (0, fs)
  where
    psi :: (Score, [Frame]) -> Maybe (Score, (Score, [Frame]))
    psi (_,   [])   = Nothing
    psi (ttl, f:fs) = Just (ttl', (ttl', fs)) where ttl' = ttl + point f

data State = Pending | Fixed deriving (Show, Eq)

state :: Frame -> State
state f | onGoing f = Pending
        | otherwise = Fixed
  where
    onGoing :: Frame -> Bool
    onGoing (Strike  bs) = length bs < 2
    onGoing (Spare _ bs) = null bs
    onGoing (Open    ts) = length ts < 2

states :: [Frame] -> [State]
states = map state

data Frame' = Frame' { getIdx   :: FrameIdx
                     , getFrame :: Frame
                     , getScore :: Score
                     , getState :: State
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
      drawPoint (Spare [x,_] [b])     = "|" ++ show x ++ "|/|" ++ show b
      drawPoint (Spare [x,_] [])      = "|" ++ show x ++ "|/| "
      drawPoint (Open  [x,y])         = "|" ++ show x ++ "|" ++ show y ++ "| "
      drawPoint (Open  [x])           = "|" ++ show x ++ "| | "
  
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
      drawPoint (Open  [x,y])   = "|" ++ show x ++ "|" ++ show y
      drawPoint (Open  [x])     = "|" ++ show x ++ "| "

      drawScore :: Score -> String
      drawScore s = "|" ++ pad ++ scoreStr
        where scoreStr = show s
              len = length scoreStr
              pad = replicate (3 - len) ' '

gameOver :: Game -> Bool
gameOver g =  n == 10 && b
  where (n, b) = foldr phi (0, True) g
        phi :: Frame' -> (Int, Bool) -> (Int, Bool)
        phi f (n, b) = (n + 1, b && getState f == Fixed)


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
drawGame g = do
  putStrLn $ "Game is " ++ if gameOver g then "over" else "on going"
  putStr $ unlines $ showGame g

-- | Test data

test:: [Throw]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]

perfect:: [Throw]
perfect = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
