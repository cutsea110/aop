module Search where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (find, nub)
import Debug.Trace (trace)

-- | ref.) https://malv.in/posts/2021-01-09-depth-first-and-breadth-first-search-in-haskell.html
--

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

slightlyBetterBfs :: Eq a => Algo a
slightlyBetterBfs (Prob s e d) = loop [s]
  where
    loop xs | any d xs  = find d xs
            | otherwise = loop (nub $ concatMap e xs)

------------------------------------------------------------

findNumber :: Prob Int
findNumber = Prob { start = 23
                  , expand = \n -> concat [[n+2, n+5] | n < 42]
                  , isDone = (== 42)
                  }

findNumber' :: Prob Int
findNumber' = Prob { start = 23
                   , expand = \n -> trace (show n) (concat [[n+2, n+5] | n < 42])
                   , isDone = (== 42)
                   }

grid :: Prob (Int, Int)
grid = Prob { start = (0, 0)
            , expand = \(x, y) ->
                trace (show (x,y)) $ [ (x+1,y) | x < 4] ++ [ (x,y+1) | y < 4]
            , isDone = (== (4,4))
            }

data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving Show

findInTree :: BinaryTree a -> (a -> Bool) -> Prob (BinaryTree a)
findInTree start isDone = Prob start subtrees treeIsDone
  where
    subtrees Empty = []
    subtrees (Node _ l r) = [l, r]
    treeIsDone Empty = False
    treeIsDone (Node x _ _) = isDone x

exampleTree :: BinaryTree String
exampleTree = Node "Hay"
              (Node "Hay"
                (Node "Hay" Empty Empty)
                (Node "Needle A" Empty Empty))
              (Node "Needle B"
                (Node "Hay" Empty Empty)
                Empty)

findNeedle :: Prob (BinaryTree String)
findNeedle = findInTree exampleTree (("Needle"==) . take 6)
