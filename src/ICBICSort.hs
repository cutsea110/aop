module ICBICSort where
-- [3 7 1 9 6 2 5 8 4]

--  v
-- [3 7 1 9 6 2 5 8 4]            ([], 3, [], [7 1 9 6 2 5 8 4])
-- [7 3 1 9 6 2 5 8 4]            ([], 7, [3], [1 9 6 2 5 8 4])
-- [9 3 1 7 6 2 5 8 4]            ([], 7, [3 1], [9 6 2 5 8 4])
-- [9 3 1 7 6 2 5 8 4]            ([], 9, [3 1 7], [6 2 5 8 4])
-- [9 3 1 7 6 2 5 8 4]            ([], 9, [3 1 7 6 2 5 8 4], [])

--    v
-- [9 3 1 7 6 2 5 8 4]            ([9], 3, [], [1 7 6 2 5 8 4])
-- [3 9 1 7 6 2 5 8 4]            ([3], 9, [], [1 7 6 2 5 8 4])
-- [3 9 1 7 6 2 5 8 4]            ([3], 9, [1 7 6 2 5 8 4], [])

--      v
-- [3 9 1 7 6 2 5 8 4]            ([3 9], 1, [], [7 6 2 5 8 4])
-- [1 9 3 7 6 2 5 8 4]            ([1 9], 3, [], [7 6 2 5 8 4])
-- [1 3 9 7 6 2 5 8 4]            ([1 3], 9, [], [7 6 2 5 8 4])
-- [1 3 9 7 6 2 5 8 4]            ([1 3], 9, [7 6 2 5 8 4], [])

--        v
-- [1 3 9 7 6 2 5 8 4]            ([1 3 9], 7, [], [6 2 5 8 4])
-- [1 3 7 9 6 2 5 8 4]            ([1 3 7], 9, [], [6 2 5 8 4])
-- [1 3 7 9 6 2 5 8 4]            ([1 3 7], 9, [6 2 5 8 4], [])

--          v
-- [1 3 7 9 6 2 5 8 4]            ([1 3 7 9], 6, [], [2 5 8 4])
-- [1 3 6 9 7 2 5 8 4]            ([1 3 6 9], 7, [], [2 5 8 4])
-- [1 3 6 7 9 2 5 8 4]            ([1 3 6 7], 9, [], [2 5 8 4])
-- [1 3 6 7 9 2 5 8 4]            ([1 3 6 7], 9, [2 5 8 4], [])

--            v
-- [1 3 6 7 9 2 5 8 4]            ([1 3 6 7 9], 2, [], [5 8 4])
-- [1 2 6 7 9 3 5 8 4]            ([1 2 6 7 9], 3, [], [5 8 4])
-- [1 2 3 7 9 6 5 8 4]            ([1 2 3 7 9], 6, [], [5 8 4])
-- [1 2 3 6 9 7 5 8 4]            ([1 2 3 6 9], 7, [], [5 8 4])
-- [1 2 3 6 7 9 5 8 4]            ([1 2 3 6 7], 9, [], [5 8 4])
-- [1 2 3 6 7 9 5 8 4]            ([1 2 3 6 7], 9, [5 8 4], [])

--              v
-- [1 2 3 6 7 9 5 8 4]            ([1 2 3 6 7 9], 5, [], [8 4])
-- [1 2 3 5 7 9 6 8 4]            ([1 2 3 5 7 9], 6, [], [8 4])
-- [1 2 3 5 6 9 7 8 4]            ([1 2 3 5 6 9], 7, [], [8 4])
-- [1 2 3 5 6 7 9 8 4]            ([1 2 3 5 6 7], 9, [], [8 4])
-- [1 2 3 5 6 7 9 8 4]            ([1 2 3 5 6 7], 9, [8 4], [])

--                v
-- [1 2 3 5 6 7 9 8 4]            ([1 2 3 5 6 7 9], 8, [], [4])
-- [1 2 3 5 6 7 8 9 4]            ([1 2 3 5 6 7 8], 9, [], [4])
-- [1 2 3 5 6 7 8 9 4]            ([1 2 3 5 6 7 8], 9, [4], [])

--                  v
-- [1 2 3 5 6 7 8 9 4]            ([1 2 3 5 6 7 8 9], 4, [], [])
-- [1 2 3 4 6 7 8 9 5]            ([1 2 3 4 6 7 8 9], 5, [], [])
-- [1 2 3 4 5 7 8 9 6]            ([1 2 3 4 5 7 8 9], 6, [], [])
-- [1 2 3 4 5 6 8 9 7]            ([1 2 3 4 5 6 8 9], 7, [], [])
-- [1 2 3 4 5 6 7 9 8]            ([1 2 3 4 5 6 7 9], 8, [], [])
-- [1 2 3 4 5 6 7 8 9]            ([1 2 3 4 5 6 7 8], 9, [], [])

--           [nil, snoc]
--   [a] <---------------- 1 + [a] * a
--    |                      |
--  u |                      | id + u * id_a
--    V                      V
--    b  <---------------- 1 + b *  a
--           [c, f]
data SList a = SNil
             | SCons (SList a) a
             deriving Show

fold :: (b, (b, a) -> b) -> SList a -> b
fold (c, f) = u
  where u SNil = c
        u (SCons xs x) = f (u xs, x)


--           out
--   [a] ----------------> 1 + [a] * a
--    A                      A
--  v |                      | id + v * id_a
--    |                      |
--    b  ----------------> 1 + b *  a
--           psi
unfold :: (b -> Maybe (b, a)) -> b -> SList a
unfold psi = v
  where v x = case psi x of
          Nothing -> SNil
          Just (b, a) -> SCons (v b) a

insert :: Ord a => a -> SList a -> SList a
insert x xs = fold (SCons SNil x, swapSnoc) xs

swapSnoc :: Ord a => (SList a, a) -> SList a
swapSnoc (SNil, x) = SCons SNil x
swapSnoc (SCons xs x, y) | x <= y    = SCons (SCons xs x) y
                         | otherwise = SCons (SCons xs y) x


--           [nil, snoc]
--   [a] <---------------- 1 + [a] * a
--    |                      |
--  u |                      | id + (id * u) * id_a
--    V                      V
--    b  <---------------- 1 + ([a] * b) *  a
--           [d, g]
para :: (b, ((SList a, b), a) -> b) -> SList a -> b
para (d, g) = u
  where u SNil = d
        u (SCons xs x) = g ((xs, u xs), x)

--           out
--   [a] ----------------> 1 + [a] * a
--    A                      A
--  v |                      | id + (id + v) * id_a
--    |                      |
--    b  ----------------> 1 + ([a] + b) *  a
--           psi
apo :: (b -> Maybe (Either (SList a) b, a)) -> b -> SList a
apo psi = v
  where v b = case psi b of
          Nothing -> SNil
          Just (Left  xs, x) -> SCons xs x
          Just (Right xs, x) -> SCons (v xs) x

sHead :: SList a -> Maybe a
sHead = fold (Nothing, g)
  where g (Nothing, x) = Just x
        g (b,       _) = b

sTail :: SList a -> SList a
sTail = apo psi
  where psi :: SList a -> Maybe (Either (SList a) (SList a), a)
        psi SNil = Nothing
        psi (SCons SNil x) = Nothing
        psi (SCons (SCons SNil y) x) = Just (Left SNil, x)
        psi (SCons ys x) = Just (Right ys, x)
