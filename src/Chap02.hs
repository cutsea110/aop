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
-- == {- 合成は可換 -}
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
-- == {- 合成は可換 -}
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
-- == {- 合成は可換 -}
--  h . (f . g) = k . (f . g)
-- == {- f . g がエピ -}
--  h = k
--
-- 双対: f . g がモノなら g がモノ
--
--  g . h = g . k
-- == {- Leibniz -}
--  f . (g . h) = f . (g . k)
-- == {- 合成は可換 -}
--  (f . g) . h = (f . g) . k
-- == {- f . g がモノ -}
--  h = k
--
-- | Ex 2.6
--
-- 任意の射はただ最大でも1本しかない。(ex. 1 <= 3 などは 1 本, 5 <= 2 などは 0 本)
-- 任意の射 m : x <= y に対して任意の f, g : w <= x をとると
-- f = g == m . f = m . g なので任意の射 m はモノ
-- 同様に e : x <= y に対して任意の f, g : y <= w をとると
-- f = g == f . e = g . e なので任意の射 e はエピ
--
-- すべての射はモノかつエピだが同型射とは限らない
-- h : x <= x な射は h 自身が逆射となる
-- h . h = id
-- それ以外に逆射はない
--
-- | Ex 2.7
--
-- エピの定義を確認する.
-- e : C -> B , 任意の f, g : B -> A に対して f = g == f . e = g . e なら e はエピ
--
-- 上への関係 e のなかで f != g でも f . e = g . e となる e を見つければ良い
-- {0, 1} -> {0, 1} なる関係を考える.
-- e : {(0, 0), {0, 1}} とすると e は上への関係である.
-- S : {(0, 1), (1, 0)} および id {(0, 0), (1, 1)} に対して
-- S . e = {(0, 1), (0, 0)} = e
-- id . e = {(0, 0), (0, 1)} = e
-- なので S . e = id . e ではあるが S != id である.
-- したがって上への関係でもエピとはかぎらない.
--
-- Rel におけるエピ射は部分関数か?
-- No.
-- {0, 1, 2} -> {0, 1} な関係において e = {(0, 0), (0, 1), (2, 0), (1, 1)} を考える.
-- e は部分関数ではないがエピである.
-- f による 0 からの値域を f0, 1 からの値域を f1, g による 0 からの値域を g0, 1 からの値域を g0 とする.
-- すると f . e と g . e が同じになるということは 2 から到達する f0 と g0 が等しく, 1 から到達する f1 と g1 が等しく,
-- さらに 0 から到達する f0 \/ f1 と g0 \/ g1 が等しくなる.
-- つまり f . e = {(2, f_0), (0, f_0 \/ f_1), (1, f_1)} = g . e = {(2, g_0), (0, f_0 \/ f_1), (1, f_1)} なので
-- f_0 = g_0, f_1 = g_1, f_0 \/ f_1 = g_0 \/ g_1 である.
-- したがって f = {(0, f_0), (1, f_1)}, g = {(0, g_0), (1, g_1)} が等しくなり, f = g である.
-- 逆に f = g なら f_0 = g_0, f_1 = g_1 なので同様にポイントワイズに比較すれば f . e = g . e.
--
-- | Ex 2.8
--
--         h
--  A <---------- C
--  |             |
-- f|             |g
--  |             |
--  v             v
--  B <---------- D
--         k
--  (h, k) . (p, q) = (h, k) . (p', q') == (p, q) = (p', q')
-- ==
--  (h . p, k . q) = (h . p', k . q') == (p, q) = (p', q')
-- ==
--  h . p = h . p' and k . q = k . q' == p = p' and q = q'
--
--  つまり h とk がそれぞれ元の圏でモノであれば Arr(A) でモノである.
--
-- | Ex 2.9
--
-- i : A <- B を同型射とすると j : B <- A が存在し, i . j = id
--  Fi . Fj
-- == {- 関手則 -}
--  F(i . j)
-- == {- i . j = id -}
--  F(id)
-- == {- 関手則 -}
--  id
--
-- また逆に i が同型射なら j . i = id なので双対性より F j . F i = id となり Fi も同型射である.
--
-- | Ex 2.10
--
-- 前順序集合の間の関手
-- A <= B を F(A) <= F(B) に移す.
-- つまり単調関数.
--
-- | Ex 2.11
--
-- 圏Cにおける対象AとBの間の射の集合が圏Hにおける対象H(A,B)だから
-- 圏Hにおける対象は積圏のようなものだと考えられる
-- 一方圏Hにおける射は以下のように考える
-- 圏Cにおける射 f : A' <- A と 射 h : B' <- B を考えると,
-- 圏Hにおける対象 H(A, B) = {g | g : A <- B in C} とすると
-- この対象 H(A, B) に射 H(f, h) を適用するとH(A',B') = f . g . h に移る.
-- 図式に示すと以下.
--
--       g
--  A <----- B
--  |        |
--f |        | h
--  |        |
--  v        v
--  A' <---- B'
--   f . g . h
--
-- 合成してみる
--       g
--  A <----- B
--  |        |
--f1|        | h0
--  |        |
--  v        v
--  A' <---- B'
--  |        |
--f0|        | h1
--  |        |
--  v        v
--  A' <---- B'
--
--  H(f0 . f1, h0 . h1) g
-- ==
--  f0 . f1 . g . h0 . h1
-- ==
--  H(f0, h1) (f1 . g . h0)
-- ==
--  H(f0, h1) (H(f1, h0) g)
-- ==
--  (H(f0, h1) . H(f1, h0)) g
--
-- したがって, H : Fun <- C x C^op となる関手である.
--
-- | Ex 2.12
--
data Tree a = Tip a
            | Bin (Tree a, Tree a)
            deriving Show

--       [Tip, Bin]
-- Ta <----------- a + Ta x Ta
-- |                 |
-- | u               | 1 + u x u
-- |                 |
-- v                 v
-- X  <----------- a + X x X
--      [c, f]

foldt :: (a -> b, (b, b) -> b) -> Tree a -> b
foldt (f, g) = u
  where u (Tip x) = f x
        u (Bin (l, r)) = g (u l, u r)

--           [Tip, Bin]
-- Ta <--------------------- a + Ta x Ta
-- |                           |
-- | mapt f                    | 1 + mapt f x mapt f
-- |                           |
-- v                           v
-- Tb <--- b + Tb x Tb <---- a + Tb x Tb
-- [Tip, Bin]       f + id x id
-- [Tip, Bin] . (f + id x id)
-- [Tip . f, Bin . (id x id)]

mapt :: (a -> b) -> Tree a -> Tree b
mapt f = foldt (Tip . f, Bin)

-- | Ex 2.13
--
-- ???
--
-- | Ex 2.14
--          swap
-- (A, B) <------ (B, A)                A      B
--  |  |           |  |                 |      |
-- g| f|           |f |g                |g     |f
--  |  |           |  |                 |      |
--  v  v           v  v                 v      v
-- (C, D) <------ (D, C)                C      D
--          swap
--
-- (g x f) . swap = swap (f x g)
--
-- g: A -> C と f: B -> D とから f x g へと移す関手を (x) とする
-- すると g x f へと移す関手を (*) とすると swap の自然性条件は (x) -> (*) への自然変換となる.
-- ただし T * S = S x T である.
--
-- | Ex 2.15
--
tau :: a -> [a]
tau x = [x]

-- tau :: P <- id
--          tau
--   PA <------- A
--    |          |
-- Pf |          | f
--    |          |
--    v          v
--   PB <------- B
--         tau
--   tau . f $ a
-- ==
--  tau (f a)
-- ==
--  [f a]
--
--  Pf . tau $ a
-- ==
--  Pf (tau a)
-- ==
--  Pf [a]
-- ==
--  [f a]
--
-- J_t :: JE <- id
--         J_t
--  JEA <------- A
--    |          |
-- JER|          |R
--    |          |
--    v          v
--  JEB <------- B
--         J_t
--
-- まず定義から確認する
-- E : Fun <- Rel
-- 対象AをPAへ写し射Rを存在像関数へ写す
-- (ER) x = {a | exists b. aRb /\ b <- x}
-- 例えば R = {(0,0),(1,0)} とすると,
-- (ER) 0 = [0, 1]
--
-- J : Rel <- Fun
-- 対象を変えないまま関数を対応する対の集合に写す.
-- 例えば f 0 = [0, 1] なら
-- Jf = {([0, 1], 0)}
--
-- ???
--
-- | Ex 2.16
--
-- cp : [[a]] <- [[a]]
-- cp : Set [a] <- [Set a]
-- cp : Set ([] a) <- [] (Set a)
--
-- | Ex 2.17
--
-- F, G : A <- B
-- H : B <- C
-- phi : F <- G
-- 定義: (phi H)A = phi (HA)
--
--        A               B           C
----------------------------------------
--      phi Hc
--  FHc <----- GHc        Hc          c
--   |          |         |           |
--FHf|          |GHf      |Hf         |f
--   |          |         |           |
--   v          v         v           v
--  FHc'<----- GHc'       Hc'         c'
--      phi Hc'
--
-- phi H : FH <- GH
--
--  FHf . (phi H)c
-- == {- phi H の定義 -}
--  FHf . phi (Hc)
-- == {- phi : F <- G -}
--  phi (Hc') . GHf
-- == {- phi H の定義 -}
--  (phi H)c' . GHf
--
-- | Ex 2.18
--
--     head
--  a <---- [a]
--  |        |
-- f|        |listr f
--  |        |
--  v        v
--  b <---- [b]
--     head
--
-- 任意の部分関数fについて
--   f . head == head . listr f
-- か?という議論.
--
-- f = recip
--  where recip x = 1 / x
-- とすると
--
--  recip (head [1, 0])
-- ==
--  recip 1
-- ==
--  1 / 1
-- ==
--  1
--
--  head (listr recip [1,0])
-- ==
--  head [recip 1, recip 0]
-- ==
--  head undefined
-- ==
--  undefined
--
-- eager evaluation 前提?
--
-- | Ex 2.19
--
--  分かりにくいので読み替える
--  圏 C^D は関手 C <- D を対象にとり,自然変換を射とする.
--  F, G : C <- D とすると関手 F および G が対象.
--  下右図を可換にする phi が射となる.
--
--      D                      C
--   -------      ----------------------------
--
--                            phi_a
--      a            Fa <------------- Ga
--      |            |                 |
--      |            |                 |
--     f|          Ff|                 |Gf
--      |            |                 |
--      v            v                 v
--      b            Fb <------------- Gb
--                            phi_b
--
--  D としていわゆる 2 という圏を取れと言っている.
--
--      2                      C
--   -------      -----------------------------
--                            phi_.
--      .            F. <------------- G.
--      |            |                 |
--      |            |                 |
--     f|          Ff|                 |Gf
--      |            |                 |
--      v            v                 v
--      x            Fx <------------- Gx
--                            phi_x
-- これは 2.8 における Arr(C) である.
-- いわゆる射圏で Arr(C) は C^{->} とも書き,
-- 2 から C への関手のなす圏である.
--
-- | 2.20
--
-- 任意の要素を e : A <- 1 とする.
-- 任意の f, g : 1 <- B に対して e . f = e . g だと言えれば良い
-- だが 1 <- B はそもそも !_B ただ一つしかないので当然成立するよって
-- 任意の要素は定数であることが言える.
--
-- B が少なくとも1つの要素 e' : B <- 1 を持つとする.
-- 任意の定数 c : A <- B は A のある要素 e について c = e . !_B と分解可能であることを示す.
--
-- (定数 c は A のある特定の要素を指す(特定の要素に潰す)ような射だとイメージしておく)
--
-- c : A <- B が定数なので,
--        c           id_B
--  A <------- B <------------- B
--               <----- 1 <---- B
--                  e'      !_B
--  上の図式においても(f := id_B, g := e' . !_B とすると)
--    c = c . id_B = c . (e' . !_B)
--  と言える.
--  結合則から c . (e' . !_B) = (c . e') . !_B
--  B が少なくとも1つの要素を持つとしてその要素を e' としたときに定義 e = c. e' と定義すると
--  c = e . !_B と書ける
--
