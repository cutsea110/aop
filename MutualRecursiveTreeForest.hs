module MutualRecursiveTreeForest where

data Tree a = Fork a (Forest a) deriving (Show, Eq)
data Forest a = Null
              | Grows (Tree a) (Forest a)
              deriving (Show, Eq)

foldt (g, c, h) (Fork x fs) = g x (foldf (g, c, h) fs)

foldf (g, c, h) Null = c
foldf (g, c, h) (Grows ts fs) = h (foldt (g, c, h) ts) (foldf (g, c, h) fs)

unfoldt b@(phi, psi) t = case phi t of
  (a, f') -> Fork a (unfoldf b f')

unfoldf b@(phi, psi) f = case psi f of
  Nothing -> Null
  Just (t', f') -> Grows (unfoldt b t') (unfoldf b f')

-- trivials

idt = foldt (Fork, Null, Grows)
idf = foldf (Fork, Null, Grows)

(idt', idf') = (unfoldt (phi, psi), unfoldf (phi, psi))
  where
    phi (Fork a f) = (a, f)
    psi Null = Nothing
    psi (Grows t f) = Just (t, f)

(genT, genF) = (unfoldt (phi, psi), unfoldf (phi, psi))
  where
    phi n = (n, n-1)
    psi n = if n <= 0 then Nothing else Just (n, n-1)

-- utility
(lenT, lenF) = (foldt (g, c, h), foldf (g, c, h))
  where
    g _ f = 1 + f
    c = 0
    h l r = l + r

(depthT, depthF) = (foldt (g, c, h), foldf (g, c, h))
  where
    g _ f = 1 + f
    c = 0
    h l r = max l r

(sumT, sumF) = (foldt (g, c, h), foldf (g, c, h))
  where
    g a f = a + f
    c = 0
    h l r = l + r

-- type functor

mapt f (Fork a fs) = Fork (f a) (mapf f fs)
mapf f Null = Null
mapf f (Grows ts fs) = Grows (mapt f ts) (mapf f fs)

(mapt', mapf') = (genMap foldt, genMap foldf)
  where
    genMap cata f = cata (g, c, h)
      where
        g = curry (uncurry Fork . cross (f, id))
        c = Null
        h = curry (uncurry Grows . cross (id, id))
        cross (f, g) (x, y) = (f x, g y)

-- sample tree and forest
t1 = Fork 1 Null
f1 = Grows t1 Null
t2 = Fork 2 f1
f2 = Grows t2 f1
t3 = Fork 3 f2
f3 = Grows t3 f2
t4 = Fork 4 f3
f4 = Grows t4 f3
t5 = Fork 5 f4
f5 = Grows t5 f4
