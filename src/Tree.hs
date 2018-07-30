{-# LANGUAGE LambdaCase #-}
module Tree where

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
wrap x = [x]
cons (x, xs) = x:xs

tips = foldt (wrap, cat)

tipcat = curry cat . tips

tips' t = foldt (curry cons, compose) t []

para (d, g) (Tip x) = d x
para (d, g) (Bin l r) = g (l, para (d, g) l) (r, para (d, g) r)

fixT f = \case
  Tip x -> Tip x
  Bin xs ys -> Bin (f xs) (f ys)

idT = fixT idT

instance Functor Tree where
  fmap f (Tip a) = Tip (f a)
  fmap f (Bin l r) = Bin (fmap f l) (fmap f r)
  a <$ (Tip _) = Tip a
  a <$ (Bin l r) = Bin (a <$ l) (a <$ r)

instance Applicative Tree where
  pure = Tip
  Tip f <*> Tip a = Tip (f a)
  Bin f g <*> Bin l r = Bin (f <*> l) (g <*> r)

-- | ref.) https://stackoverflow.com/questions/6798699/monad-instance-for-binary-tree
-- and answered by Edward Kmett
instance Monad Tree where
  return = pure
  Tip a >>= f  = f a
  Bin l r >>= f = Bin (l >>= f) (r >>= f)

eta = {- alpha . in = [tip, bin] . inl = -} tip
mu = {- (| id, alpha . inr|) = (| [id, [tip, bin] . inr] |) = -} foldt (id, bin)
