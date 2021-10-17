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

data List list = Nil | Cons K list deriving Show

type K = Int

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons k x) = Cons k (f x)

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
upcast = fold (unfold (fmap out))
upcast' :: (Functor f) => Mu f -> Nu f
upcast' = fold out'
upcast'' :: (Functor f) => Mu f -> Nu f
upcast'' = unfold (fold (fmap In))
upcast''' :: (Functor f) => Mu f -> Nu f
upcast''' = unfold in'
