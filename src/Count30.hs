module Count30 where

import Data.List (unfoldr)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout, hClose)

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  interact (unlines . player 30 . lines)
  putStrLn "Thanks for playing!"

data Mode = Play Int | Continue

player :: Int -> [String] -> [String]
player stones inputs = "The rest is 30. You can get 1-3 stones. Your turn.":unfoldr psi (Play stones, inputs)
  where
    psi :: ((Mode, [String]) -> Maybe (String, (Mode, [String])))
    psi (_, []) = Nothing
    psi (Continue, c:cs)
      | c `elem` ["y", "Y"] = Just ("Start new game. You can get 1-3 stones. Your turn.", (Play stones, cs))
      | otherwise = Nothing
    psi (Play n, x:xs)
      | y `notElem` [1,2,3] = Just ("Invalid input. You can get 1-3 stones. Your turn.", (Play n, xs))
      | y > n               = Just ("You can only get up to " ++ show n ++ " stones. Your turn.", (Play n, xs))
      | otherwise           = case m `divMod` 4 of
          (q, 0) | q > 0     -> Just ("I got 3" ++ ", then the rest is " ++ show (m-3) ++ ". Your turn.", (Play (m-3), xs))
                 | otherwise -> Just ("You lose!. Continue? (y/n)", (Continue, xs))
          (q, 1) -> Just ("I got 1, then the rest is " ++ show (m-1) ++ ". Your turn.", (Play (m-1), xs)) -- TODO: randomise
          (q, 2) -> Just ("I got 1, then the rest is " ++ show (m-1) ++ ". Your turn.", (Play (m-1), xs))
          (q, 3) -> Just ("I got 2, then the rest is " ++ show (m-2) ++ ". Your turn.", (Play (m-2), xs))
      where y = read x :: Int
            m = n - y
