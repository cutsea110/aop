module Leafy where

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

data Leafy a = Tip a | Bin (Leafy a) (Leafy a) deriving Show

tip = Tip
bin = uncurry Bin

foldt (f, g) (Tip x) = f x
foldt (f, g) (Bin l r) = g (foldt (f, g) l, foldt (f, g) r)

mapt f = foldt (tip.f, bin.cross(id,id))
eta = tip
mu = foldt (id,bin)

instance Functor Leafy where
    fmap = mapt

instance Applicative Leafy where
    pure = eta
    Tip f <*> x = fmap f x
    Bin l r <*> x = bin (l <*> x, r <*> x)

instance Monad Leafy where
    return = eta
    m >>= f = mu (fmap f m)
