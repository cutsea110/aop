module TypeFunctor where

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

-- F(A,T) = 1 + A * T
-- F(f,g) = 1 + f * g
data List a = Nil
            | Cons a (List a)
            deriving (Show, Eq)

fold (c, f) = u
  where
    u Nil = c
    u (Cons x xs) = f (x, u xs)

nil = Nil
cons = uncurry Cons

-- list f = fold (alpha . F(f, id))
--        = fold ([nil, cons] . (id + f * id))
list f = fold (nil, cons . cross (f, id))

-- F(A,T) = A + T * T
-- F(f,g) = f + g * g
data BTree a = Tip a
             | Bin (BTree a) (BTree a)
             deriving (Show, Eq)

tip = Tip
bin = uncurry Bin

foldt (f, g) = u
  where
    u (Tip a) = f a
    u (Bin l r) = g (u l, u r)

-- tree f = foldt (alpha . F(f, id))
--        = foldt ([tip, bin] . (f + id * id))
tree f = foldt (tip . f, bin . cross (id, id))

{--
-- Tt * Tf = (A * Tf) " (1 + Tt * Tf)
-- F(A,Tt,Tf) = (A * Tf) * (1 + Tt * Tf)
-- F(f,g,h) = (f * h) * (1 + g * h)
-- F(f,id,id) = (f * id) * (1 + id * id)
data Tree a = Fork a (Forest a) deriving (Show, Eq)
data Forest a = Nulls
              | Grows (Tree a) (Forest a)
              deriving (Show, Eq)

fork = uncurry Fork
nulls = Nulls
grows = uncurry Grows

foldt (g, c, h) (Fork x fs)  = g (x, foldf (g, c, h) fs)

foldf (g, c, h) Nulls        = c
foldf (g, c, h) (Grows t fs) = h (foldt (g, c, h) t, foldf (g, c, h) fs)

-- mapt f = foldt (alpha . ((f * id) * (1 + id * id)))
--        = foldt ((fork * [nulls, grows]) . ((f * id) * (1 + id * id)))
--        = foldt (fork . cross (f, id), nulls, grows . cross (id, id))
-- mapf f = foldf (alpha . ((f * id) * (1 + id * id)))
--        = foldf ((fork * [nulls, grows]) . ((f * id) * (1 + id * id)))
--        = foldf (fork . cross (f, id), nulls, grows . cross (id, id))

-- mapt f = foldt (fork . cross (f, id), nulls, grows . cross (id, id))
-- mapf f = foldf (fork . cross (f, id), nulls, grows . cross (id, id))
(mapt, mapf) = (gen foldt, gen foldf)
  where
    gen cata f = cata (g, c, h)
      where
        g = fork . cross (f, id)
        c = nulls
        h = grows . cross (id, id)
--}

-- F(f,g) = f + G(g)
-- F(A,B) = A + G(B)
-- phi = alpha . inl, psi = cata (id, alpha . inr)
-- F(A,B) = A + B * B
--
-- eta = [tip, bin] . inl = tip
eta = tip
-- mu  = cata (id, [tip, bin] . inr) = cata (id, bin)
mu = foldt (id, bin)

instance Functor BTree where
  fmap = tree

instance Applicative BTree where
  pure = eta
  Tip f <*> x = fmap f x
  Bin l r <*> x = bin (l <*> x, r <*> x)

instance Monad BTree where
  return = eta
  m >>= f = mu (fmap f m)
