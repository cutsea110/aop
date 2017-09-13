module Tree where

data Tree a = Tip a | Bin (Tree a) (Tree a) deriving (Show, Eq)

foldt (f, g) (Tip a) = f a
foldt (f, g) (Bin tl tr) = g (foldt (f, g) tl) (foldt (f, g) tr)

unfoldt phi x = case phi x of
  Left a -> Tip a
  Right (tl, tr) -> Bin (unfoldt phi tl) (unfoldt phi tr)

gen = unfoldt phi
  where
    phi n = if n <= 0 then Left n else Right (n-1, n-1)

mapt f (Tip a) = Tip (f a)
mapt f (Bin l r) = Bin (mapt f l) (mapt f r)

mapt' f = foldt (Tip . f, Bin <$> id <$> id)

tips = foldt (wrap, cat)
  where
    cat x y = x ++ y
    wrap x = [x]
