module CombPlus where

import Control.Monad
import Data.List

import Combinatorial (perms)

br 0 = [[],[0]]
br n = nub $ concatMap (map sort . up) (br (n-1))

up :: Num a => [a] -> [[a]]
up [] = [[1]]
up (n:ns) = (n+1:ns):map (n:) (up ns)

pr xs = forM_ xs pr'
  where
    pr' = putStrLn . concat . intersperse " + " . map show

br' = nub . concatMap perms . br
