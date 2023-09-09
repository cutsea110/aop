module Dijkstra where

import Prelude hiding (map, foldr)

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
