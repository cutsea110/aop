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

mapt f = foldt (tip.f, bin.cross(id,id))
etat = tip
mut = foldt (id,bin)

instance Functor Leafy where
    fmap = mapt

instance Applicative Leafy where
    pure = etat
    -- (<*> x) = foldt ((<$> x), bin) = (|(<$> x), inr|)
    fs <*> x = foldt ((<$> x), bin) fs

instance Monad Leafy where
    return = etat
    m >>= f = mut (f <$> m)


data NonEmptyList a = Single a | Cons a (NonEmptyList a) deriving Show

single = Single
cons = uncurry Cons

foldnel :: (a -> b, (a, b) -> b) -> NonEmptyList a -> b
foldnel (f, g) = u
  where
    u (Single x)  = f x
    u (Cons x xs) = g (x, u xs)

mapnel :: (a -> b) -> NonEmptyList a -> NonEmptyList b
mapnel f = foldnel (single.f, cons.cross(f,id))
