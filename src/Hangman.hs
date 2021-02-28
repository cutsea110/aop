module Hangman where

import Data.Char (toLower)

hangman :: IO ()
hangman = do
  xs <- readFile "Words"
  play (words xs)

play :: [String] -> IO ()
play (w:ws) = do
  putStrLn "I am thinking of a word:"
  putStrLn (replicate (length w) '-')
  putStrLn "Try and guess it."
  guess w ws

guess :: String -> [String] -> IO ()
guess w ws = do
  putStr "guess: "
  w' <- getLine
  if length w' /= length w
    then do
    putStrLn "Wrong number of letters!"
    guess w ws
    else do
    if w' == w then do
      putStrLn "You got it!"
      putStr "Play again? (yes or no): "
      ans <- getLine
      if ans == "yes" then play ws
        else putStrLn "Bye!"
      else do
      putStrLn (match w' w)
      guess w ws

match w' w = map check w
  where check x = if x `elem` w' then x else '_'
