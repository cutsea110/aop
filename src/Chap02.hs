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
--  つまり h と k がそれぞれ元の圏でモノであれば Arr(A) でモノである.
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
-- | Ex 2.21
--
-- 空集合が Fun の始対象.
-- 同じく Rel における始対象も空集合.
-- Fun x Fun における始対象は 空集合と空集合の対.
-- (積圏の場合は射も要素ごとなので)
--
-- | Ex 2.22
--
-- 最大値が存在する
--
-- | Ex 2.23
-- 前問と同様に積圏における始対象や終対象は始対象の対と終対象の対.
-- 0_AxB = (0_A, 0_B)
-- 1_AxB = (1_A, 1_B)
--
-- | Ex 2.24
--
-- Ex 2.19 によりこの圏は関手圏である.
--  関手圏は関手を対象とし自然変換を射とする.
--
--  この場合の終対象となる関手 * : C -> D は *(X) -> * *(f) -> id_* となる.
--
--    C                 D
--  -----  -----------------------------
--              phi     psi     !
--    a     Fa ---> Ga ---> Ha ---> *
--    |     |       |       |       |
--    |     |       |       |       |id
--    v     v       v       v       v
--    b     Fb ---> Gb ---> Hb ---> *
--
-- | Ex 2.25
--
--        x
--       /|\
--      / | \
--   <=/  |  \<=
--    /   |   \
--   /    |    \
--  v     v      v
-- 4 <--- p ---> 6
--    >=     <=
--
-- pが積であるとは,任意のxについて
--
-- x <= p === x <= 4 and x <= 6
--
-- であること,つまり積pは4と6の小さい方 min(a,b)
--
--        x
--       ^^^
--      / | \
--   >=/  |  \>=
--    /   |   \
--   /    |    \
--  /     |     \
-- 4 ---> p <--- 6
--    <=     >=
--
-- pが余積であるとは,任意のxについて
-- 
-- p <= x === 4 <= x and 6 <= x
--
-- であること,つまり余積pは4と6の大きい方 max(a,b)
--
-- | Ex 2.26
--
-- 自然同型であることを示すには同型射だと示せばよい.
-- つまり逆射の存在を示せばよい
--
unit :: (a, ()) -> a
unit (x, ()) = x

counit :: a -> (a, ())
counit x = (x, ())

prop_unit x = (unit . counit) x == x
prop_counit x = (counit . unit) x == x

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

coswap :: (b, a) -> (a, b)
coswap = swap

prop_swap xy = (coswap . swap) xy == xy

assocr :: ((a, b), c) -> (a, (b, c))
assocr ((x, y), z) = (x, (y, z))

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (x, (y, z)) = ((x, y), z)

prop_assocr xyz = (assocl . assocr) xyz == xyz
prop_assocl xyz = (assocr . assocl) xyz == xyz

-- | Ex 2.27
--
-- <[f,g],[h,k]> == [<f,h>,<g,k>]
--   
--
-- outl . <f, g> = f       outl (pair (f, g) (x, y)) = outl (f x, g y) = f x
-- outr . <f, g> = g       outr (pair (f, g) (x, y)) = outr (f x, g y) = g y
--
-- [f, g] (inl a) = f a    either (f, g) (Left a)  = f a
-- [f, g] (inr b) = g b    either (f, g) (Right b) = g b
--
--
-- splitの普遍性 (積の普遍性)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--  h = <f, g> == outl . h = f and outr . h = g  (2.4)
-- から
--  <[f,g],[h,k]> == [<f,h>,<g,k>]
--     ===
--  outl . [<f,h>,<g,k>] = [f,h] /\ outr . [<f,h>,<g,k>] = [h,k]
-- を示す.
--
--  outl . [<f,h>,<g,k>]
-- == {- 余積の融合則 m . [f, g] = [m . f, m . g] -}
--  [outl . <f, h>, outl . <g, k>]
-- == {- outl . <f, g> = f -}
--  [f, g]
--
--  同様に
--
--  outr . [<f,h>,<g,k>]
-- == {- 余積の融合則 m . [f, g] = [m . f, m . g] -}
--  [outr . <f,h>, outr . <g,k>]
-- == {- outr . <f, g> = g -}
--  [h, k]
--
-- | Ex 2.28
--
-- m がモノとは任意の f, g について
--   f = g == m . f = m . g
--
-- e がエピとは任意の f, g について
--   f = g == f . e = g . e
--
-- outl,outr がエピか?
--  f = g == f . outl = g . outl
--
--      f     outl
--  x <--- a <--- a * b
--    <---
--      g
--
--  f = g => f . outl = g . outl
--  f . outl = g . outl => f = g
--
-- よって成り立つ. outr も同様.
--
-- inl,inr がモノか?
--  f = g == inl . f = inl . g
--
--         inl     f
--  a + b <--- a <--- x
--               <---
--                 g
--
--  f = g => inl . f = inl . g
--  inl . f = inl . g => f = g
-- よって成り立つ. inr も同様.
--
-- | Ex 2.29
--
-- Arr(A) は 圏A の射が対象となるような圏
-- すると積は射の対なので
--
--       A            Arr(A)
--    -------       ---------
--        g
--    A <--- B          g
--    |      |          |
--   k|      |h         |(h,k)
--    v      v          v
--    C <--- D          f
--        f
--  つまり元の圏Aにおいて上の可換図で k.g=f.h なる(h,k)がArr(A)における射で(h,k):f <- g
--
--   圏A内でどうなっているから図式に表すと以下のようになる.
--
--                     f
--                 A <--- B
--                /|\    /|\
--               / | \  / | \
--              /  |  \/  |  \
--             /   |  /\  |   \
--            /    | /  \ |    \
--           /     |/    \|     \
--          /      /      \      \
--         /      /|      |\      \
--        k0    h0 j      i k1     h1
--       /      /  |      |  \      \
--      /      /   |      |   \      \
--     v      v    v g0*g1v    v      v
--     C <--- D   C*E <- D*F   E <--- F
--     ^  g0  ^   | |    | |   ^   g1 ^
--     | outl |   | |outr| |   |      |
--     `------|---' `----|-|---'      |
--            `----------' `----------'
--              outl           outr
--
--   (h0, k0) : g0 <- f
--   (h1, k1) : g1 <- f
--   (i, j) : g0*g1 <- f
--
--   (i, j) : g0*g1 <- f は i : <h0, h1> と j : <k0, k1> になるので (i, j) : (<h0, h1>, <k0, k1>) となる.
--   よって (<h0, h1>, <k0, k1>) : g0 * g1 <- f
--
-- | Ex 2.30
--
-- (outl . <f, g>) a = f a を確認するというのが本文における問題
--
-- ただし
--  <f, g> a = inl (f a) -- f a 定義, g a 未定義
--           = inr (g a) -- f a 未定義, g a 定義
--           = mid (f a, g a) -- それ以外
--
-- f a が未定義, g a が定義(本文中のケース)
-- <f, g> a = inr (g a)
-- outl (<f, g> a) = outl (inr (g a)) = undefined
--
-- f a が定義, g a が未定義
-- <f, g> a = inl (f a)
-- outl (<f, g> a) = outl (inl (f a)) = f a
--
-- f a が未定義, g a が未定義
-- <f, g> a = mid (f a, g a)
-- outl (<f, g> a) = outl (mid (f a, g a)) = f a (未定義だけどね)
--
-- f a が定義, g a が定義
-- <f, g> a = mid (f a, g a)
-- outl (<f, g> a) = outl (mid (f a, g a)) = f a
--
-- outr の場合はどうか
-- (outr . <f, g>) a = g a を確認する
--
-- f a が未定義, g a が定義
-- <f, g> a = inr (g a)
-- outr (<f, g> a) = outr (inr (g a)) = g a
--
-- f a が定義, g a が未定義
-- <f, g> a = inl (f a)
-- outr (<f, g> a) = outr (inl (f a)) = undefined
--
-- f a が未定義, g a が未定義
-- <f, g> a = mid (f a, g a)
-- outr (<f, g> a) = outr (mid (f a, g a)) = g a(未定義だけどね)
--
-- f a が定義, g a が定義
-- <f, g> a = mid (f a, g a)
-- outr (<f, g> a) = outr (mid (f a, g a)) = g a
--
-- | Ex 2.31
--
-- h  = Pair undefined undefined : Pair A B <- C
-- h' = undefined : Pair A B <- C
--
-- とすると, outl . h = undefined = outl . h' /\ outr . h = undefined = outr . h'
-- したがって積の図式を可換するような h がただ 1 本に決まらない.(同型でもない)
-- よって No.
--
-- | Ex 2.32
--
-- f0 : A <- B * A
-- f1 : A <- A * C
-- f2 : A <- B
--
-- g  : A <- F(A)
--
-- phi0 g = f0
-- phi1 g = f1
-- phi2 g = f2
--
-- F(A) = ((B * A) + (A * C)) + B
-- g = [[f0, f1], f2]
--
-- これは f0, f1, f2 のドメインの型の直和型になっている
--
-- phi0 g = g . inl . inl
-- phi1 g = g . inl . inr
-- phi2 g = g . inr
--
f0 :: (b, a) -> a
f0 = undefined
f1 :: (a, c) -> a
f1 = undefined
f2 :: b -> a
f2 = undefined

g :: Either (Either (b, a) (a, c)) b -> a
-- g (Right b)             = f2 b
-- g (Left (Left (b, a)))  = f0 (b, a)
-- g (Left (Right (a, c))) = f1 (a, c)
g = either (either f0 f1) f2

-- | Ex 2.33
--
-- 恒等関手Iの場合F代数 X <- I(X) の始代数aは T <- I(T) で以下の図式が可換となるようなもので
-- 任意のX <- I(X)へただ1本射uがあるようなもの
--
--      a
--  T <--- I(T) = T
--  |       |
-- u|       |I(u)
--  v       v
--  X <--- I(X) = X
--
-- すると T はこの圏における始対象なので0でよい.
-- 始代数は id_0.
--
-- | Ex 2.34
--
-- cata の普遍性を再確認しておく.
--  h = (|f|) == h . a = f . F(h)
--
-- 図式を書いてみると以下のようになる.
--         a
--     T <--- F(T)
--    ^|       |^
--  m'||m  F(m)||F(m')
--    |v       v|
--     X <--- F(X)
--         f
--
-- m が cata である,特に m が (|f|) であるということは m . a = f . F(m) であることと同じ.(cata の普遍性より)
-- よって m . a = f . F(m) であることを示せればよい.
-- 上の図式から f = m . a . F(m') と置くと良さそうと予想できる.
--
--  f = m . a . F(m')
-- =>
--  m . a = (m . a . F(m')) . F(m)
-- ==
--  m . a = m . a . F(m') . F(m)
-- ==
--  m . a = m . a . F(m' . m)
-- ==
--  m . a = m . a . F(id)
-- ==
--  m . a = m . a
--
-- つまり f = m . a . F(m') とすると m . a = f . F(m) という cata の普遍性を満たすことが証明できる.
-- よって m は (|f|) つまり (|m . a . F(m')|) だと確認できた.
--
-- | Ex 2.35
--
-- (|f . g|) = f . (|g . Ff|) を示す.
--
-- 融合則
-- h . (|f|) = (|g|) <= h . f = g . Fh から
--
-- f . (g . Ff) = (f . g) . Ff
-- ~   ~~~~~~~~   ~~~~~~~    ~
-- h       f         g       h
-- =>
-- f . (|g . Ff|) = (|f . g|)
-- となる.
--
-- 図式で示すと以下.
--
--   f : Y <- X
--   g : X <- FY
--
--                    a = In
--              T <------------FT
--             /|             / |
--            / |            /  |
--    u=(|f.g|) |         Fu/   |
--          /   v=(|g.Ff|) /    |Fv
--         /    |         /     |
--        v     v        v      v
--       Y <--- X <--- FY <--- FX
--           f      g       Ff
--
-- | Ex 2.36
--
--  m = <f, id> および m' = outr となるように取ると m' . m = id_T なので Ex 2.34 から m は cata にできる.
--  図式に示すと以下.
--                       a
--              - T <--------- FT
--             / ^|            |
--            /  ||            |
--           /   ||            |
--         f/    ||(|g|)       |F(|g|)
--         / outr||            |
--        /      ||            |
--       v       |v            v
--       A <---- A*T <----- F(A*T)
--         outl         g
--
--  g : A * T <- F(A * T) とすれば f = outl . (|g|) とできる.
--
-- | Ex 2.37
--
-- 冪関手 P を考えれば良い.
--
-- P では元の集合の圏Setsにおける始対象つまり空集合からでも{[]}へと移るため空集合が存在しなくなる.
--
-- | Ex 2.38
--
--       a
--   T <--- B * T
--   |        |
--  u|        | 1 * u
--   v        v
--   A <--- B * A
--       f
--
-- | Ex 2.38
--
-- ???
--
-- | Ex 2.39
--
-- 双関手 F(A,B) に対応する始型 (a, T) とする.
-- LA = F(GA,HA) と定義する.
--
-- 命題
--   phi : H <- L => (|phi|) : H <- TG
--
--             a
--       TGA <--- F(GA, TGA)
--        |           |
-- (|phi|)|           | F(id, (|phi|))
--        v           v
--        HA <--- F(GA, HA) == LA
--            phi
--
-- 図式の通りだが論証では以下を使う.
--
-- 融合則
-- h . (|f|) = (|g|) <= h . f = g . Fh から
--
-- 型関手融合 (2.14)
-- (|h|) . Tg = (|h . F(g, id)|)
--
--   phi : H <- L
-- == {- phiの自然性 -}
--   Hf . phi = phi . Lf
-- == {- Lの定義 -}
--   Hf . phi = phi . F(Gf, Hf)
-- == {- 関手則 -}
--   Hf . phi = phi . F(Gf, id) . F(id, Hf)
--  ~~~   ~~~   ~~~~~~~~~~~~~~~   ~~~~~~~~~~
--   h     f         g               Fh
-- => {- 融合則 -}
--   Hf . (|phi|) = (|phi . F(Gf, id)|)
--                    ~~~     ~~
--                     h       g
-- == {- 型関手融合 -}
--   Hf . (|phi|) = (|phi|) . TGf
-- == {- (|phi|)の自然性 -}
--   (|phi|) : H <- TG
--
-- | Ex 2.40
--
-- H : A <- A がモナドとは
-- mu . Heta = id = mu . eta /\ mu . mu = mu . Hmu を満たすこと.
-- ただし eta : H <- id および mu : H <- HH なる自然変換とする.
--
-- F(f,g) = f + Gg
--
-- (a, T) を F の始型とする.
-- phi = a . inl, psi = (|id, a . inr|) とすると (T, phi, psi) がモナドであることを示す.
--
-- 対応は eta => phi, mu => psi および H => T である.
--
-- 1. phi : T <- id
-- 2  psi : T <- TT
-- 3. psi . Tphi = id = psi . phi
-- 4. psi . psi = psi . Tpsi
--
-- 1. phi : T <- id
--
-- (1) Tf . a = a . F(f, Tf)
--             a
-- A     T_A <--- F(A, T_A)
-- |      |           |
-- |f   Tf|           |F(f, Tf)
-- v      v           v
-- B     T_B <--- F(B, T_B)
--             a
-- (2) inl : (+) <- outl
--
--              (+)関手
--        +----------------- (A, C)
--        |        +---------/
--        v    inl v   outl関手
--      A + C <--- A 
--        |        |
--     f+g|        |f
--        v        v
--      B + D <--- B
--             inl
--
--  (1) より
--  Tf . a = a . F(f, Tf)
-- == {- ライプニッツ -}
--  Tf . a . inl = a . F(f, Tf) . inl
-- == {- F(f, g) = f + Gg -}
--  Tf . a . inl = a . (f + GTf) . inl
-- == {- inl : (+) <- outl -}
--  Tf . a . inl = a . inl . f
-- == {- phi の定義 -}
--  Tf . phi = phi . f
--
-- よって phi : T <- id
--
-- 2. psi : T <- TT
--
-- Ex 2.39 によると
--    Fを双関手, (a, T)をその始型とすると, LA = F(GA, HA) を定義(G,H単項関手)したら,
--    phi : H <- L => (|phi|) : H <- TG
--
-- よって,この G, H を T にとると, phi : T <- F(TA, TA) => (|phi|) : T <- TT が言えることになる.
-- この (|phi|) が本問における psi となれば良いはず.((|phi|) の phi は Ex 2.39 の phi なので混同しないように)
-- この (Ex 2.39 でいうところの) phi は本問でいえば [id, a . inr] なので, [id, a . inr] : T <- F(TA, TA) ならば証明できる.
-- つまり次式が証明できれば良い.
--
--  Tf . [id, a . inr] = [id, a . inr] . F(Tf, Tf)
--
-- (1)
--           a
--     T_A <--- F(A, T_A)
--      |         |
--    Tf|         |F(f, Tf)
--      v         v
--     T_B <--- F(B, T_B)
--           a
--
-- (2) inr : (+) <- outr
--
--              (+)関手
--        +----------------- (A, C)
--        |        +---------/
--        v    inr v   outr関手
--      A + C <--- C 
--        |        |
--     f+g|        |g
--        v        v
--      B + D <--- D
--             inr
--
--  Tf . [id, a . inr]
-- == {- ケース融合 "2.5 余積"を参照 -}
--  [Tf, Tf . a . inr]
-- == {- Tは始型, cata (1) -}
--  [Tf, a . F(f, Tf) . inr]
-- == {- Fの定義 -}
--  [Tf, a . (f + GTf) . inr]
-- == {- inr : (+) <- outr (2) -}
--  [Tf, a . inr . GTf]
-- == {- ケース消去則 "2.5 余積"を参照 -}
--  [id, a . inr] . (Tf + GTf)
-- == {- Fの定義 -}
--  [id, a . inr] . F(Tf, Tf)
--
-- これにより [id, a . inr] が T <- F(TA, TA) な自然変換であることが示されたので、
-- Ex 2.39 から (|id, a . inr|) すなわち psi は T <- TT な自然変換である
--
-- 3. psi . Tphi = id = psi . phi
--
--  psi . Tphi
-- == {- psi の定義 -}
--  (|id, a . inr|) . Tphi
-- == {- 型関手融合 (|h|) . Tg = (|h . F(g, id)|) -}
--  (|[id, a . inr] . F(phi, id)|)
-- == {- F(f, g) = f + Gg -}
--  (|[id, a . inr] . (phi + Gid)|)
-- == {- 余積の融合則 -}
--  (|phi, a . inr . Gid|)
-- == {- phi の定義 -}
--  (|a . inl, a . inr . Gid|)
-- == {- 余積と余積の融合則を逆に -}
--  (|a . [inl, inr] . (id + Gid)|)
-- == {- F(f, g) = f + Gg -}
--  (|a . [inl, inr] . F(id, id)|)
-- == {- 余積の反射則 "2.5 余積" -}
--  (|a . F(id, id)|)
-- == {- 双関手の反射則 "2.2 の関手の例" -}
--  (|a|)
-- == {- cata の反射則 -}
--  id
--
--  psi . phi
-- == {- phi の定義 -}
--  psi . a . inl
-- == {- psi の定義 -}
--  (|id, a . inr|) . a . inl
--  ~~~~~~~~~~~~~~~
--         h
-- == {- (2.10) の cata の普遍性 h = (|f|) == h . a = f . Fh から -}
--  [id, a . inr] . F(id, psi) . inl
-- == {- F(f, g) = f + Gg -}
--  [id, a . inr] . (id + Gpsi) . inl
-- == {- 余積の融合則 -}
--  [id, a . inr . Gpsi] . inl
-- == {- 余積の普遍性 -}
--  id
--
-- 4. psi . psi = psi . Tpsi
--
--  psi . psi = psi . Tpsi
-- == {- psi の定義 -}
--  psi . psi = (|id, a . inr|) . Tpsi
-- == {- 型関手融合 (|h|) . Tg = (|h . F(g, id)|) -}
--  psi . psi = (|[id, a . inr] . F(psi, id)|)
-- == {- psi の定義 -}
--  psi . (|id, a . inr|) = (|[id, a . inr] . F(psi, id)|)
--  ~~~     ~~~~~~~~~~~       ~~~~~~~~~~~~~~~~~~~~~~~~~~
--   h          f                      g
-- <= {- 融合 (2.12) h . (|f|) = (|g|) <= h . f = g . Fh  -}
--  psi . [id, a . inr] = [id, a . inr] . F(psi, id) . F(id, psi)
-- == {- Fは双関手 -}
--  psi . [id, a . inr] = [id, a . inr] . F(psi, psi)
-- == {- F(f, g) = f + Gg -}
--  psi . [id, a . inr] = [id, a . inr] . (psi + Gpsi)
-- == {- 余積の融合則(分配) -}
--  [psi, psi . a . inr] = [psi, a . inr . Gpsi]
-- <= {- 余積の融合則 -}
--  psi . a . inr = a . inr . Gpsi
-- == {- 下の図式 -}
--  true
--                 a            inr
--           TTA <--- TA + GTTA <--- GTTA
--            |          /|            |
--            |         / |            |
--            |        /  |            |
--            |       /   |            |
-- psi=(|phi|)|      /    |            | Gpsi
--            |    inl    |            |
--            |    /  F(id,(|phi|))    |
--            |   /    = id + G(|phi|) |
--            |  /        |            |
--            v v         v            v
--            TA <---- TA + GTA <---- GTA
--             ^   phi            inr  |
--              \_                     |
--                \__________________  |inr
--                         a         \ |
--                                    \v
--                                  A + GTA
--
