module Chap02 where

-- | Ex 2.1
-- f . u = f
--        f          u
-- B <------- A <-------- A
-- B <------------------- A
--            f
--
-- f は任意なので f として id_A をとる
--
--  id_A . u
-- == {- f . u = f -}
--  id_A
--
-- 一方 id_A は単位元なので
--  id_A . u
-- == {- id_A は単位元 -}
--  u
--
-- ゆえに id_A . u == id_A かつ id_A . u == u なので u == id_A
--

-- | Ex 2.2
--
--        f
--    <---------
--  A ---------> B
--  |     h     /
--  |          /
-- g|         /k
--  |        /
--  v       /
--  C<-----/
--
-- k . h . f . h : C <- A
-- g . k . h     : type error
--
-- | Ex 2.3
--
-- Def. r : A <- B は r . r' = id_A なる r': B <- A が存在するならレトラクションという
--          r
--     A <----- B
--     ^       ^
--     |      /
--   id|     / r'
--     |    /
--     A --/
-- 
-- Theorem. r : A <- B がレトラクションならば任意の f : A <- C に対して g : B <- C が存在して r . g = f である
--          r
--     A <----- B
--     ^       ^^
--     |    r'/ |
--   id|     /  | g
--     |    /   |
--     A --+    C
--       <-----
--          f
-- Prove:
--  g = r' . f ととる
--
--  r . g
-- == {- g = r' . f -}
--  r . r' . f
-- == {- r はレトラクション -}
--  id_A . f
-- == {- 単位元 -}
--  f
--
-- レトラクションの双対
--   Def. s : B <- A は s' . s = id_A なる s' が存在するならセクションという
--         s
--     A -----> B
--     |       /
--   id|      / s'
--     |     /
--     v    /
--     A <-/
--
--   Theorem. s : B <- A がセクションならば任意の f : C <- A に対して g : C <- B が存在して g . s = f である
--
--         s
--     A -----> B
--     |       /|
--   id|    s'/ | g
--     |     /  |
--     v    /   v
--     A <-+    C
--       ----->
--         f
-- | Ex 2.4
-- m : B -> A , 任意の f, g : C -> B に対して f = g == m . f = m . g なら m はモノ
-- e : C -> B , 任意の f, g : B -> A に対して f = g == f . e = g . e なら e はエピ
--
-- f . g = id なら g はモノ
--
--  g . h = g . k
-- == {- Leibniz -}
--  f . (g . h) = f . (g . k)
-- == {- 結合は可換 -}
--  (f . g) . h = (f . g) . k
-- == {- f . g = id -}
--  id . h = id . k
-- == {- 単位元 -}
--  h = k
--
--
-- f . g = id なら f はエピを示す
--
--  h . f = k . f
-- == {- Leibniz -}
--  (h . f) . g = (k . f) . g
-- == {- 結合は可換 -}
--  h . (f . g) = k . (f . g)
-- == {- f . g = id -}
--  h . id = k . id
-- == {- 単位元 -}
--  h = k
--
-- | Ex 2.5
--
-- f . g がエピなら f がエピ
--
--  h . f = k . f
-- == {- Leibniz -}
--  (h . f) . g = (k . f) . g
-- == {- 結合は可換 -}
--  h . (f . g) = k . (f . g)
-- == {- f . g がエピ -}
--  h = k
--  