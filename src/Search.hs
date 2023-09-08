module Search where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (find)

data Prob a = Prob { start :: a
                   , expand :: a -> [a]
                   , isDone :: a -> Bool
                   }

type Algo a = Prob a -> Maybe a


dfs :: Algo a
dfs (Prob s e d) = loop s
  where
    loop x | d x       = Just x
           | otherwise = listToMaybe $ mapMaybe loop (e x)

bfs :: Algo a
bfs (Prob s e d) = loop [s]
  where
    loop xs | any d xs  = find d xs
            | otherwise = loop (concatMap e xs)
