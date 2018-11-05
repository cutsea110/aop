module BinaryTree where

cross3 (f, g, h) (x, y, z) = (f x, g y, h z)

data BTree1 a = Empty
              | Node a (BTree1 a) (BTree1 a)
              deriving (Show, Eq)

empty = Empty
node (x, l, r) = Node x l r

foldbt1 (c, f) Empty = c
foldbt1 (c, f) (Node x l r) = f (x, foldbt1 (c, f) l, foldbt1 (c, f) r)

mapbt1 :: (a -> b) -> BTree1 a -> BTree1 b
mapbt1 f = foldbt1 (empty, node . cross3 (f, id, id))

eta x = node (x, empty, empty)
mu = foldbt1 (empty, f)
  where
    f (Empty, Empty, Empty) = empty
    f (Node x l r, ls, rs) = undefined
    f (Empty, Node x l r, rs) = undefined
    f (Empty, Empty, Node x l r) = undefined
