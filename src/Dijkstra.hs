module Dijkstra where

import Prelude hiding (map, foldr)
import Data.List (nub, delete)

-- | グラフアルゴリズムの構成的定義と変換に関する研究 by 篠埜 功
-- ref.) https://www.cs.ise.shibaura-it.ac.jp/~sasano/pub/master.pdf
--

-- | Nat
data Nat = Zero | Succ Nat deriving Show

foldn (c, f) = u
  where u Zero     = c
        u (Succ n) = f (u n)

plus n = foldn (n, Succ)
mult n = foldn (Zero, plus n)
expn n = foldn (Succ Zero, mult n)

-- | List
data List a = Nil | Cons (a, List a) deriving Show

foldr (c, f) = u
  where u Nil            = c
        u (Cons (x, xs)) = f (x, u xs)

map f = foldr (Nil, h)
  where h (x, xs) = Cons (f x, xs)

-- | Tree
data Tree a = Tip a | Bin (Tree a, Tree a) deriving Show

foldt (f, g) = u
  where u (Tip x)      = f x
        u (Bin (l, r)) = g (u l, u r)

size  = foldt (const 1, uncurry (+))
depth = foldt (const 1, \(l, r) -> 1 + max l r)


{-- | Graph
data Graph = Empty
           | Edge
           | Vert Int Int -- In / Out
           | Swap Int Int -- FirstHalf / SecondHalf
           | Pack Graph Graph
           | Join Graph Graph
           deriving Show
-}

-- | Graph
data Graph = Empty | Context :&: Graph deriving Show
type Context = ([Vertex], Vertex, [Vertex])
type Vertex = String
infixr 5 :&:


ufold :: (Context -> a -> a) -> a -> Graph -> a
ufold f g = u
  where u Empty = g
        u (c :&: h) = f c (u h)

greverse :: Graph -> Graph
greverse = ufold (\(p,v,s) r -> (s,v,p) :&: r) Empty

gmember :: Vertex -> Graph -> Bool
gmember u = ufold (\(p,v,s) r -> u == v || r) False

g28 = (["d"],"a",["b"]) :&: ([],"b",["c","d"]) :&: ([],"c",[]) :&: ([],"d",[]) :&: Empty
g32 = (["4","5"],"6",["2"]) :&: (["2","3"],"5",[]) :&: (["2"],"4",["1"]) :&: (["1"],"3",["2"]) :&: (["1"],"2",[]) :&: ([],"1",[]) :&: Empty

depthfold :: (Context -> a -> a -> a) -> a -> [Vertex] -> Graph -> a
depthfold f u vs = fst . depthfold' f u vs

depthfold' :: (Context -> a -> a -> a) -> a -> [Vertex] -> Graph -> (a, Graph)
depthfold' f u []     g = (u, g)
depthfold' f u (v:vs) g
  | gmember v g = let ((p,n,s), g') = apm v g -- (p,n,s) :&: g' = apm v g
                      (r1,g1) = depthfold' f u s g'
                      (r2,g2) = depthfold' f u vs g1
                  in (f (p,n,s) r1 r2, g2)
  | otherwise   = depthfold' f u vs g

data Tree' a = Node a [Tree' a] deriving Show
dfs :: [Vertex] -> Graph -> [Tree' Vertex]
dfs = depthfold f []
  where f (p,n,s) r1 r2 = Node n r1 : r2

breadthfold :: (Context -> a -> a -> a) -> a -> [Vertex] -> Graph -> a
breadthfold f u vs = fst . breadthfold' f u vs

breadthfold' :: (Context -> a -> a -> a) -> a -> [Vertex] -> Graph -> (a, Graph)
breadthfold' f u []     g = (u, g)
breadthfold' f u (v:vs) g
  | gmember v g = let ((p,n,s), g') = apm v g -- (p,n,s) :&: g' = apm v g
                      (r1,g1) = breadthfold' f u vs g'
                      (r2,g2) = breadthfold' f u s g1
                  in (f (p,n,s) r1 r2, g2)
  | otherwise   = breadthfold' f u vs g

bfs :: [Vertex] -> Graph -> [Tree' Vertex]
bfs = breadthfold f []
  where f (p,n,s) r1 r2 = Node n r2 : r1

-- | apm: Active Pattern Matching
--   指定された頂点およびその頂点に付随する枝と、それらを除いたグラフとに分割する
apm :: Vertex -> Graph -> (Context, Graph)
apm v g = sub g ([],[])
  where sub Empty (p,s) = ((p,v,s), Empty)
        sub ((p1,v1,s1) :&: g1) (p,s)
          | v == v1   = sub g1 (p1++p, s1++s)
          | otherwise = let ((p2,_,s2), g2) = sub g1 (p,s)
                            (p3,s2') = if v `elem` p1
                                       then (delete v p1, nub $ v1:s2)
                                       else (p1,s2)
                            (s3,p2') = if v `elem` s1
                                       then (delete v s1, nub $ v1:p2)
                                       else (s1,p2)
                        in ((p2',v,s2'), (p3,v1,s3) :&: g2)

