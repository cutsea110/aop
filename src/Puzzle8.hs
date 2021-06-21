module Puzzle8 where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray)
import Data.Array.MArray

type Position = (Int, [Int])
data Move = U | D | L | R deriving (Show, Eq)
type Path = ([Move], Position)
type Frontier = [Path]

n = 100019

solve :: Position -> Maybe [Move]
solve start = runST $ do
  pa <- newArray (0, n-1) []
  bfs pa [([], start)] []

bfs :: STArray s Int [Position] -> Frontier -> Frontier -> ST s (Maybe [Move])
bfs pa [] [] = return Nothing
bfs pa [] mqs = bfs pa mqs []
bfs pa ((ms, p):mps) mqs
  = if solved p then return (Just (reverse ms))
    else do { ps <- readArray pa k
            ; if p `elem` ps then bfs pa mps mqs
              else do { writeArray pa k (p:ps)
                      ; bfs pa mps (succs (ms, p) ++ mqs)
                      }
            }
  where k = hash p

hash :: Position -> Int
hash p = fromInteger (encode p) `mod` n

succs :: Path -> [Path]
succs (ms, p) = [(m:ms, move p m) | m <- moves p]

moves :: Position -> [Move]
moves (j, ks)
  =    [U | j `notElem` [6, 7, 8]]
    ++ [D | j `notElem` [0, 1, 2]]
    ++ [L | j `notElem` [2, 5, 8]]
    ++ [R | j `notElem` [0, 3, 6]]

move :: Position -> Move -> Position
move (j, ks) U = (j+3, swap (j, j+3) ks)
move (j, ks) D = (j-3, swap (j-3, j) ks)
move (j, ks) L = (j+1, swap (j, j+1) ks)
move (j, ks) R = (j-1, swap (j-1, j) ks)

swap (j, k) ks = ks1 ++ y:ks3 ++ x:ks4
  where (ks1, x:ks2) = splitAt j ks
        (ks3, y:ks4) = splitAt (k-j-1) ks2

solved :: Position -> Bool
solved p = p == (8, [1,2,3,4,5,6,7,8,0])

encode :: Position -> Integer
encode (j, ks) = foldl op 0 ks
  where op x d = 10*x + fromIntegral d

start :: Position
start = (0, [0..8])
