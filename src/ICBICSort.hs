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

--         [nil, s, p, t]
--   [a] <---------------- 1 + a + a * a + a * [a] * a
--    |                      |
--  u |                      | id + id + id * id + id * u * id
--    V                      V
--    b  <---------------- 1 + a + a * a + a * b * a
--           [c, f, g, h]

data List a = Nil
            | Single a
            | Pair a a
            | Node a (List a) a
            deriving Show

cata :: (b, a -> b, (a, a) -> b, (a, List a, a) -> b) -> List a -> b
cata (c, f, g, h) = u
  where u Nil          = c
        u (Single e)   = f e
        u (Pair x y)   = g (x, y)
        u (Node x y z) = h (x, y, z)

--            out
--   [a] ----------------> 1 + a + a * a + a * [a] * a
--    A                      A
--  u |                      | id + id + id * id + id * u * id
--    |                      |
--    b  ----------------> 1 + a + a * a + a * b * a
--            psi
data Tri a b c = L a | C b | R c deriving Show

ana :: (b -> Maybe (Tri a (a, a) (a, b, a))) -> b -> List a
ana psi = v
  where v x = case psi x of
          Nothing            -> Nil
          Just (L x)         -> Single x
          Just (C (x, y))    -> Pair x y
          Just (R (x, y, z)) -> Node x (v y) z

nil :: List a
nil = Nil

cons :: a -> List a -> List a
cons w = cata (c, f, g, h)
  where c = Single w
        f x = Pair w x
        g (x, y) = Node w (Single x) y
        h (x, y, z) = Node w (cons x y) z

snoc :: a -> List a -> List a
snoc w = cata (c, f, g, h)
  where c = Single w
        f x = Pair x w
        g (x, y) = Node x (Single y) w
        h (x, y, z) = Node x (snoc z y) w


uncons :: List a -> Maybe (a, List a)
uncons = cata (c, f, g, h)
  where c = Nothing
        f x = Just (x, Nil)
        g (x, y) = Just (x, Single y)
        h (x, y, z) = Just (x, snoc z y)

unsnoc :: List a -> Maybe (List a, a)
unsnoc = cata (c, f, g, h)
  where c = Nothing
        f x = Just (Nil, x)
        g (x, y) = Just (Single x, y)
        h (x, y, z) = Just (cons x y, z)

bubble :: Ord a => List a -> List a
bubble = cata (c, f, g, h)
  where
    c = Nil
    f x = Single x
    g (x, y) | x <= y    = Pair x y
             | otherwise = Pair y x
    h (x, Nil, z) | x <= z    = Pair x z
                  | otherwise = Pair z x
    h (x, Single y, z) | x <= y    = Node x (Single y) z
                       | otherwise = Node y (Single x) z
    h (x, Pair y z, v) | x <= y    = Node x (Pair y z) v
                       | otherwise = Node y (Pair x z) v
    h (x, Node y z v, w) | x <= y    = Node x (snoc v (cons y z)) w
                         | otherwise = Node y (snoc v (cons x z)) w

insert :: Ord a => List a -> List a
insert = ana swapUncons
  where swapUncons :: Ord a => List a -> Maybe (Tri a (a, a) (a, List a, a))
        swapUncons Nil = Nothing
        swapUncons (Single x) = Just (L x)
        swapUncons (Pair x y) | x <= y    = Just (C (x, y))
                              | otherwise = Just (C (y, x))
        swapUncons (Node x Nil z) | x <= z    = Just (C (x, z))
                                  | otherwise = Just (C (z, x))
        swapUncons (Node x (Single y) z) | x <= y    = Just (R (x, Single y, z))
                                         | otherwise = Just (R (y, Single x, z))
        swapUncons (Node x (Pair y z) w) | x <= y    = Just (R (x, Pair y z, w))
                                         | otherwise = Just (R (y, Pair x z, w))
        swapUncons (Node x (Node y z v) w) | x <= y    = Just (R (x, Node y z v, w))
                                           | otherwise = Just (R (y, Node x z v, w))
