{-# LANGUAGE LambdaCase #-}
module Tree where

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
compose (h, k) = h . k

data Tree a = Tip a | Bin (Tree a) (Tree a) deriving (Show, Eq)
tip = Tip
bin = uncurry Bin

foldt :: (a -> b, (b, b) -> b) -> Tree a -> b
foldt (f, g) (Tip a) = f a
foldt (f, g) (Bin tl tr) = g (foldt (f, g) tl, foldt (f, g) tr)

unfoldt :: (b -> Either a (b, b)) -> b -> Tree a
unfoldt phi x = case phi x of
  Left a -> tip a
  Right (tl, tr) -> bin (unfoldt phi tl, unfoldt phi tr)

gen = unfoldt phi
  where
    phi n = if n <= 0 then Left n else Right (n-1, n-1)

mapt f (Tip a) = tip (f a)
mapt f (Bin l r) = bin (mapt f l, mapt f r)

mapt' f = foldt (tip . f, bin . cross (id, id))

cat (x, y) = x ++ y
wrap = (:[])
cons = uncurry (:)

tips = foldt (wrap, cat)

tipcat = curry cat . tips

tips' t = foldt (curry cons, compose) t []

para (d, g) (Tip x) = d x
para (d, g) (Bin l r) = g (l, para (d, g) l) (r, para (d, g) r)

fixT f = \case
  Tip x -> tip x
  Bin xs ys -> bin (f xs, f ys)

idT = fixT idT

instance Functor Tree where
  fmap = mapt
  a <$ (Tip _) = tip a
  a <$ (Bin l r) = bin (a <$ l, a <$ r)

-- | ref.) http://www.cs.nott.ac.uk/~pszvc/g52afp/functor_applicative.hs
instance Applicative Tree where
  pure = eta
  Tip f <*> x = fmap f x
  Bin l r <*> x = bin (l <*> x, r <*> x)

-- | ref.) https://stackoverflow.com/questions/6798699/monad-instance-for-binary-tree
-- and answered by Edward Kmett
instance Monad Tree where
  return = pure
  m >>= f = mu (fmap f m)
--  Tip a >>= f  = f a
--  Bin l r >>= f = Bin (l >>= f) (r >>= f)

test = do
  x <- bin (tip 1,tip 2)
  y <- bin (tip 'A',tip 'B')
  return (x, y)
test' = bin (tip 1,tip 2) >>= \x -> bin (tip 'A',tip 'B') >>= \y -> return (x, y)
test2 = do
  x <- bin (tip 1, bin (tip 2, tip 3))
  y <- bin (tip 'A', tip 'B')
  return (x, y)
test2' = bin (tip 1, bin (tip 2, tip 3)) >>= \x -> bin (tip 'A', tip 'B') >>= \y -> return (x, y)
test3 = do
  x <- bin (tip 1, bin (tip 2, tip 3))
  y <- bin (bin (tip 'A', tip 'B'), tip 'C')
  return (x, y)
test3' = bin (tip 1, bin (tip 2, tip 3)) >>= \x -> bin (bin (tip 'A', tip 'B'), tip 'C') >>= \y -> return (x, y)

sample = bin (tip (bin (tip (bin (tip 1, tip 2)), tip (bin (tip 3, tip 4)))), tip (bin (tip (bin (tip 5, tip 6)), tip (bin (tip 7, tip 8)))))


-- bad implementation
-- eta = bin . pair (eta, eta)
--
-- a.k.a return
-- >>> let t2 = bin (tip 1, tip 2)
-- >>> t2
-- Bin (Tip 1) (Tip 2)
-- >>> eta t2
-- Tip (Bin (Tip 1) (Tip 2))
-- >>> fmap eta t2
-- Bin (Tip (Tip 1)) (Tip (Tip 2))
-- >>> mu . eta $ t2
-- Bin (Tip 1) (Tip 2)
-- >>> mu . fmap eta $ t2
-- Bin (Tip 1) (Tip 2)
eta = {- alpha . inl = [tip, bin] . inl = -} tip
-- a.k.a join
-- >>> let t1 = tip (bin (tip (tip 1), tip (bin (tip 2, tip 3))))
-- >>> t1
-- Tip (Bin (Tip (Tip 1)) (Tip (Bin (Tip 2) (Tip 3))))
-- >>> mu t1
-- Bin (Tip (Tip 1)) (Tip (Bin (Tip 2) (Tip 3)))
-- >>> mu . mu $ t1
-- Bin (Tip 1) (Bin (Tip 2) (Tip 3))
-- >>> fmap mu t1
-- Tip (Bin (Tip 1) (Bin (Tip 2) (Tip 3)))
-- >>> mu . fmap mu $ t1
-- Bin (Tip 1) (Bin (Tip 2) (Tip 3))
mu = {- (| id, alpha . inr |) = (| id, [tip, bin] . inr |) = -} foldt (id, bin)
