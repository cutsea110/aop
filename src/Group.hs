module Group where

import Control.Monad (forM_)
import Data.List (sortOn, nub, sort)
import Text.Printf (printf)

import Combinatorial (perms)

type S4Op = String

toName :: Int -> S4Op
toName n = "s" ++ show n

s4 :: [(S4Op, [Int])]
s4 = zip s4Names (perms [1..4])

g :: [[Int]]
g = [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]]

leftCosets, rightCosets :: [[S4Op]]
leftCosets  = nub $ map (\op -> sort $ map (flip compose op . opNameOf) g) s4Names
rightCosets = nub $ map (\op -> sort $ map (compose op . opNameOf) g) s4Names
leftEqualRight op = l == r
  where l = map (\op' -> apply (compose op  op') [1,2,3,4]) gNames
        r = map (\op' -> apply (compose op' op ) [1,2,3,4]) gNames
        gNames = map opNameOf g

compose :: S4Op -> S4Op -> S4Op
compose op1 op2 = opNameOf . apply op1 . apply op2 $ [1,2,3,4]

s4Names :: [S4Op]
s4Names = fmap toName [0..23]

op :: S4Op -> [Int]
op name = case lookup name s4 of
  Just xs -> xs
  Nothing -> error $ "op: " ++ name ++ " not found"

complement :: S4Op -> S4Op
complement = opNameOf . map fst . sortOn snd . zip [1..] . op

-- | g . f . g'
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
      printf "%4s" (compose l r)
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
