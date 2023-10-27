module Group where

import Control.Monad (forM_)
import Data.List (sortOn)
import Text.Printf (printf)

import Combinatorial (perms)

type S4Op = String

toName :: Int -> S4Op
toName n = "s" ++ show n

s4 :: [(S4Op, [Int])]
s4 = zip s4Names (perms [1..4])

s4Names :: [S4Op]
s4Names = fmap toName [0..23]

op :: S4Op -> [Int]
op name = case lookup name s4 of
  Just xs -> xs
  Nothing -> error $ "op: " ++ name ++ " not found"

complement :: S4Op -> S4Op
complement name = opNameOf $ map fst $ sortOn snd $ zip [1..] (op name)

covariantOver :: S4Op -> S4Op -> [Int] -> [Int]
f `covariantOver` g = apply g . apply f . apply g'
  where
    g' = complement g

opNameOf :: [Int] -> S4Op
opNameOf xs = case lookup xs dict of
  Just name -> name
  Nothing -> error $ "opNameOf: " ++ show xs ++ " not found"
  where
    dict = map swap s4
    swap (x,y) = (y,x)

apply :: S4Op -> [Int] -> [Int]
apply name = map (\i -> op name !! pred i)

table :: IO ()
table = do
  -- header
  putHeader
  -- separator
  putSeparator
  -- lines
  forM_ s4Names $ \l -> do
    printf "%4s |" l
    forM_ s4Names $ \r -> do
      let o = apply l $ apply r [1,2,3,4]
      printf "%4s" (opNameOf o)
    putStrLn ""
  where
    putNewline = putStrLn ""
    
    putHeader = do
      printf "     |"
      forM_ s4Names $ printf "%4s"
      putNewline
      
    putSeparator = do
      putStr "-----+"
      forM_ s4Names $ \_ -> putStr "----"
      putNewline