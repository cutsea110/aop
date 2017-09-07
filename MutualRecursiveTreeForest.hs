module MutualRecursiveTreeForest where

data Tree a = Fork a (Forest a) deriving (Show, Eq)
data Forest a = Null
              | Grows (Tree a) (Forest a)
              deriving (Show, Eq)

foldt (g, c, h) (Fork x fs) = g x (foldf (g, c, h) fs)

foldf (g, c, h) Null = c
foldf (g, c, h) (Grows ts fs) = h (foldt (g, c, h) ts) (foldf (g, c, h) fs)

idt = foldt (Fork, Null, Grows)
idf = foldf (Fork, Null, Grows)

unfoldt b@(phi, psi) t = case phi t of
  (a, f') -> Fork a (unfoldf b f')

unfoldf b@(phi, psi) f = case psi f of
  Nothing -> Null
  Just (t', f') -> Grows (unfoldt b t') (unfoldf b f')

(idt', idf') = (unfoldt (phi, psi), unfoldf (phi, psi))
  where
    phi (Fork a f) = (a, f)
    psi Null = Nothing
    psi (Grows t f) = Just (t, f)

(genT, genF) = (unfoldt (phi, psi), unfoldf (phi, psi))
  where
    phi n = (n, n-1)
    psi n = if n <= 0 then Nothing else Just (n, n-1)

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
