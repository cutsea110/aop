module Leafy where

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

data Leafy a = Tip a | Bin (Leafy a) (Leafy a) deriving Show

tip = Tip
bin = uncurry Bin

foldt (f, g) = u
  where
    u (Tip x)   = f x
    u (Bin l r) = g (u l, u r)

-- T = mapt f = (|a.F(f,id)|) = (|[tip,bin]+(f+id*id)|) = (|[tip.f, bin.(id*id)]|)
mapt f = foldt (tip.f, bin.cross(id,id))
-- eta = a.inl = [tip,bin].inl = tip
etat = tip
-- mu = (|id, a.inr|) = (|id, [tip,bin].inr|) = (|id, bin|)
mut = foldt (id,bin)

instance Functor Leafy where
    fmap = mapt

instance Applicative Leafy where
    pure = etat
    -- (<*> x) = foldt ((<$> x), bin) = (|(<$> x), inr|)
    fs <*> ts = foldt ((<$> ts), bin) fs

instance Monad Leafy where
    return = pure
    m >>= f = mut (f <$> m)

(<**>) :: Monad m => m (a -> b) -> m a -> m b
fs <**> ts = do
  f <- fs
  t <- ts
  return (f t)

-------------------------

data NonEmptyList a = Single a | Cons a (NonEmptyList a) deriving Show

single = Single
cons = uncurry Cons

foldnel :: (a -> b, (a, b) -> b) -> NonEmptyList a -> b
foldnel (f, g) = u
  where
    u (Single x)  = f x
    u (Cons x xs) = g (x, u xs)

-- T = mapnel f = (|a.F(f,id)|) = (|[single,cons]+(f+f*id)|) = (|[single.f, cons.(f*id)]|)
mapnel f = foldnel (single.f, cons.cross(f,id))
-- eta = a.inl = [single,cons].inl = single
etanel = single
-- NOT mu = (|id, a.inr|) = (|id, [single,cons].inr|) = (|id, cons|)
-- munel = foldnel (id, cons)
