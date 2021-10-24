module SortOverMorphism where
-- ref.) http://www.cs.ox.ac.uk/people/daniel.james/sorting/sorting.pdf
import Data.List (partition, unfoldr, delete)

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