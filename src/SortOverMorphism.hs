{-# LANGUAGE TypeOperators #-}
module SortOverMorphism where
-- ref.) http://www.cs.ox.ac.uk/people/daniel.james/sorting/sorting.pdf
import Data.List (partition, unfoldr, delete)

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g x = (f x, g x)
cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
cross f g (x, y) = (f x, g y)

insertSort :: [Integer] -> [Integer]
insertSort = foldr insert []

insert :: Integer -> [Integer] -> [Integer]
insert y ys = xs ++ [y] ++ zs
  where (xs, zs) = partition (<y) ys
  
selectSort :: [Integer] -> [Integer]
selectSort = unfoldr select

select :: [Integer] -> Maybe (Integer, [Integer])
select [] = Nothing
select xs = Just (x, xs')
  where x = minimum xs
        xs' = delete x xs

------------------------------------------------------------------------------------

newtype Mu f = In { in' :: f (Mu f) }

fold :: (Functor f) => (f a -> a) -> Mu f -> a
fold f = f . fmap (fold f) . in'

newtype Nu f = Out' { out :: f (Nu f) }

out' :: f (Nu f) -> Nu f
out' = Out'

unfold :: (Functor f) => (a -> f a) -> (a -> Nu f)
unfold f = out' . fmap (unfold f) . f

para :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
para f = f . fmap (pair id (para f)) . in'

apo :: Functor f => (b -> f (Either (Nu f) b)) -> b -> Nu f
apo f = Out' . fmap (either id (apo f)) . f

------------------------------------------------------------------------------------

downcast :: (Functor f) => Nu f -> Mu f
downcast = In . fmap downcast . out

upcast :: (Functor f) => Mu f -> Nu f
upcast = fold (unfold (fmap out)) -- == fold out'  -- これは fold In == id だから
upcast' :: (Functor f) => Mu f -> Nu f
upcast' = unfold (fold (fmap In)) -- == unfold in' -- これは unfold out == id だから


------------------------------------------------------------------------------------

-- NOTE : instance of Ord
type K = Int

data List list = Nil | Cons K list deriving Show

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons k x) = Cons k (f x)


data SList list = SNil | SCons K list deriving Show

instance Functor SList where
  fmap f SNil = SNil
  fmap f (SCons k x) = SCons k (f x)


bubbleSort :: Mu List -> Nu SList
bubbleSort = unfold bubble
  where bubble = fold bub

bub :: List (SList (Mu List)) -> SList (Mu List)
bub Nil = SNil
bub (Cons a SNil) = SCons a (In Nil)
bub (Cons a (SCons b x))
  | a <= b    = SCons a (In (Cons b x))
  | otherwise = SCons b (In (Cons a x))


naiveInsertSort :: Mu List -> Nu SList
naiveInsertSort = fold naiveInsert
  where naiveInsert = unfold naiveIns

naiveIns :: List (Nu SList) -> SList (List (Nu SList))
naiveIns Nil = SNil
naiveIns (Cons a (Out' SNil)) = SCons a Nil
naiveIns (Cons a (Out' (SCons b x)))
  | a <= b    = SCons a (Cons b x)
  | otherwise = SCons b (Cons a x)


------------------------------------------------------------------------------------

swap :: List (SList x) -> SList (List x)
swap Nil = SNil
swap (Cons a SNil) = SCons a Nil
swap (Cons a (SCons b x))
  | a <= b    = SCons a (Cons b x)
  | otherwise = SCons b (Cons a x)

bubbleSort' :: Mu List -> Nu SList
bubbleSort' = unfold (fold (fmap In . swap))
naiveInsertSort' :: Mu List -> Nu SList
naiveInsertSort' = fold (unfold (swap . fmap out))

------------------------------------------------------------------------------------

insertSort' :: Mu List -> Nu SList
insertSort' = fold insert
  where insert = apo ins

ins :: List (Nu SList) -> SList (Either (Nu SList) (List (Nu SList)))
ins Nil = SNil
ins (Cons a (Out' SNil)) = SCons a (Left (Out' SNil))
ins (Cons a (Out' (SCons b x')))
  | a <= b    = SCons a (Left (Out' (SCons b x')))
  | otherwise = SCons b (Right (Cons a x'))

insertSort'' :: Mu List -> Nu SList
insertSort'' = fold insert
  where insert = apo (swop . fmap (pair id out))

selectSort' :: Mu List -> Nu SList
selectSort' = unfold select
  where select = para sel
sel :: List (Mu List, SList (Mu List)) -> SList (Mu List)
sel Nil = SNil
sel (Cons a (x, SNil)) = SCons a x
sel (Cons a (x, SCons b x'))
  | a <= b    = SCons a x
  | otherwise = SCons b (In (Cons a x'))

selectSort'' :: Mu List -> Nu SList
selectSort'' = unfold select
  where select = para (fmap (either id In) . swop)
  
swop :: List (x, SList x) -> SList (Either x (List x))
swop Nil = SNil
swop (Cons a (x, SNil)) = SCons a (Left x)
swop (Cons a (x, SCons b x'))
  | a <= b    = SCons a (Left x)
  | otherwise = SCons b (Right (Cons a x'))

------------------------------------------------------------------------------------

data Tree t = Empty | Node t K t deriving Show
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node l k r) = Node (f l) k (f r)

type SearchTree = Tree

pivot :: List (SearchTree (Mu List)) -> SearchTree (Mu List)
pivot Nil = Empty
pivot (Cons a Empty) = Node (In Nil) a (In Nil)
pivot (Cons a (Node l b r))
  | a <= b    = Node (In (Cons a l)) b r
  | otherwise = Node l b (In (Cons a r))

sprout :: List (x, SearchTree x) -> SearchTree (Either x (List x))
sprout Nil = Empty
sprout (Cons a (t, Empty)) = Node (Left t) a (Left t)
sprout (Cons a (t, Node l b r))
  | a <= b    = Node (Right (Cons a l)) b (Left r)
  | otherwise = Node (Left l) b (Right (Cons a r))

treeIns :: List (Nu SearchTree) -> SearchTree (Either (Nu SearchTree) (List (Nu SearchTree)))
treeIns Nil = Empty
treeIns (Cons a (Out' Empty)) = Node (Left (Out' Empty)) a (Left (Out' Empty))
treeIns (Cons a (Out' (Node l b r)))
  | a <= b    = Node (Right (Cons a l)) b (Left r)
  | otherwise = Node (Left l) b (Right (Cons a r))


grow :: Mu List -> Nu SearchTree
grow = unfold (para (fmap (either id In) . sprout))
grow' :: Mu List -> Nu SearchTree
grow' = fold (apo (sprout . fmap (pair id out)))

------------------------------------------------------------------------------------

glue :: SearchTree (Nu SList) -> SList (Either (Nu SList) (SearchTree (Nu SList)))
glue Empty = SNil
glue (Node (Out' SNil) a r) = SCons a (Left r)
glue (Node (Out' (SCons b l)) a r) = SCons b (Right (Node l a r))

wither :: SearchTree (x, SList x) -> SList (Either x (SearchTree x))
wither Empty = SNil
wither (Node (l, SNil) a (r, _)) = SCons a (Left r)
wither (Node (l, SCons b l') a (r, _)) = SCons b (Right (Node l' a r))

shear :: SearchTree (Mu SearchTree, SList (Mu SearchTree)) -> SList (Mu SearchTree)
shear Empty = SNil
shear (Node (l, SNil) a (r, _)) = SCons a r
shear (Node (l, SCons b l') a (r, _)) = SCons b (In (Node l' a r))

flatten :: Mu SearchTree -> Nu SList
flatten = fold (apo (wither . fmap (pair id out)))
flatten' :: Mu SearchTree -> Nu SList
flatten' = unfold (para (fmap (either id In) . wither))

quickSort :: Mu List -> Nu SList
quickSort = flatten . downcast . grow

treeSort :: Mu List -> Nu SList
treeSort = flatten' . downcast . grow'

------------------------------------------------------------------------------------

type Heap = Tree

pile :: List (x, Heap x) -> Heap (Either x (List x))
pile Nil = Empty
pile (Cons a (t, Empty)) = Node (Left t) a (Left t)
pile (Cons a (t, Node l b r))
  | a <= b    = Node (Right (Cons b r)) a (Left l)
  | otherwise = Node (Right (Cons a r)) b (Left l)

heapIns :: List (Nu Heap) -> Heap (Either (Nu Heap) (List (Nu Heap)))
heapIns Nil = Empty
heapIns (Cons a (Out' Empty)) = Node (Left (Out' Empty)) a (Left (Out' Empty))
heapIns (Cons a (Out' (Node l b r)))
  | a <= b    = Node (Right (Cons b r)) a (Left l)
  | otherwise = Node (Right (Cons a r)) b (Left l)

divvy :: List (Heap (Mu List)) -> Heap (Mu List)
divvy Nil = Empty
divvy (Cons a Empty) = Node (In Nil) a (In Nil)
divvy (Cons a (Node l b r))
  | a <= b    = Node (In (Cons b r)) a l
  | otherwise = Node (In (Cons a r)) b l

sift :: Heap (x, SList x) -> SList (Either x (Heap x))
sift Empty = SNil
sift (Node (l, SNil) a (r, _)) = SCons a (Left r)
sift (Node (l, _) a (r, SNil)) = SCons a (Left l)
sift (Node (l, SCons b l') a (r, SCons c r'))
  | b <= c    = SCons a (Right (Node l' b r))
  | otherwise = SCons a (Right (Node l c r'))

meld :: Heap (Mu Heap, SList (Mu Heap)) -> SList (Mu Heap)
meld Empty = SNil
meld (Node (l, SNil) a (r, _)) = SCons a r
meld (Node (l, _) a (r, SNil)) = SCons a l
meld (Node (l, SCons b l') a (r, SCons c r'))
  | b <= c    = SCons a (In (Node l' b r))
  | otherwise = SCons a (In (Node l c r'))

blend :: Heap (Nu SList, SList (Nu SList)) -> SList (Either (Nu SList) (Heap (Nu SList)))
blend Empty = SNil
blend (Node (l, SNil) a (r, _)) = SCons a (Left r)
blend (Node (l, _) a (r, SNil)) = SCons a (Left l)
blend (Node (l, SCons b l') a (r, SCons c r'))
  | b <= c    = SCons a (Right (Node l' b r))
  | otherwise = SCons a (Right (Node l c r'))

heapSort :: Mu List -> Nu SList
heapSort = unfold deleteMin . downcast . fold heapInsert
  where deleteMin = para meld
        heapInsert = apo heapIns

mingleSort :: Mu List -> Nu SList
mingleSort = fold (apo (blend . fmap (pair id out))) . downcast . unfold (fold divvy)
