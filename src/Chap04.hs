module Chap04 where

-- | Ex 4.1
--
--  X \subseteq R \cap (S \cap T)
-- = {- 交わりの普遍性 -}
--  X \subseteq R /\ X \subseteq (S \cap T)
-- = {- 交わりの普遍性 -}
--  X \subseteq R /\ (X \subseteq S /\ X \subseteq T)
-- = {- 連言は結合的 -}
--  (X \subseteq R /\ X \subseteq S) /\ X \subseteq T
-- = {- 交わりの普遍性 -}
--  X \subseteq (R \cap S) /\ X \subseteq T
-- = {- 交わりの普遍性 -}
--  X \subseteq (R \cap S) \cap T

-- | Ex 4.2
--
-- 左の図式
--  S . R \subseteq T
--
-- 右の図式
--  S . R \subseteq T /\ T . V \subseteq U
--

-- | Ex 4.3
--
-- (R . S) \cap (R . T) \subseteq R . (S \cap T)
--
-- R = {(0, 0), (0, 1)}
-- S = {(0, 0)}
-- T = {(1, 0)}
-- とすると,
--  R . S = {(0, 0)}
--  R . T = {(0, 0)}
--  なので R . S \cap R . T = {(0, 0)} = S
-- 一方,
--  S \cap T = {}
-- なので, R . (S \cap T) = {}
--

-- | Ex 4.4
--
-- 前順序の圏で考えると,射 A <- B は A >= B.
-- すると積はminに当たる.

-- | Ex 4.5
--
-- R \cap (S . T) = R \cap (S . ((S^op . R) \cap T)) を示す
--
-- モジュラ則を利用する.
--    (R . S) \cap T \subseteq R . (S \cap (R^op . T))
--
--  R \cap (S . T) \subseteq R \cap (S . ((S^op . R) \cap T))
-- = {- 交わりの普遍性 -}
--  R \cap (S . T) \subseteq R /\ R \cap (S . T) \subseteq S . ((S^op . R) \cap T)
-- = {- 交わりの下界 R \cap X \subseteq R -}
--  true /\ R \cap (S . T) \subseteq S . ((S^op . R) \cap T)
-- = {- 連言 -}
--  (S . T) \cap R \subseteq S . (T \cap (S^op . R))
-- = {- モジュラ則 -}
--  true
--
-- 逆方向の証明
--
--  R \cap (S . ((S^op . R) \cap T)) \subseteq R \cap (S . T)
-- = {- 交わりの普遍性 -}
--  R \cap (S . ((S^op . R) \cap T)) \subseteq R /\ R \cap (S . ((S^op . R) \cap T)) \subseteq S . T
-- = {- 交わりの下界 R \cap X \subseteq R -}
--  true /\ R \cap (S . ((S^op . R) \cap T)) \subseteq S . T
-- = {- 連言 -}
--  R \cap (S . ((S^op . R) \cap T)) \subseteq S . T
-- <= {- 交わりの下界 X \cap T \subseteq T -}
--  R \cap (S . T) \subseteq S . T
-- = {- 交わりの下界 X \cap S . T \subseteq S . T -}
--  true
--

-- | Ex 4.6
--
-- モジュラ則
--    (R . S) \cap T \subseteq R . (S \cap (R^op . T))
--
--  R
-- = {- 交わりの冪等則 -}
--  R \cap R
-- = {- id -}
--  (R . id) \cap R
-- \subseteq {- モジュラ則 -}
--  R . (id \cap (R^op . R))
-- = {- 単調性の公理 -}
--  R . id \cap R . (R^op . R)
-- \subseteq {- 交わりの下限 X \cap R \subseteq R -}
--  R . (R^op . R)
-- = {- 合成の結合則 -}
--  R . R^op . R
--

-- | Ex 4.7
--
-- A と B とが寓なら A x B も寓.
-- 対象を (A, B) とし射をポイントワイズに定義する.
-- (R, S) (a, b) = (c, d) は (cRa, dSb) とすれば良い.
-- 圏としてはそれで成立するとして寓に追加の演算について検討する.
--
-- 包含は R \subseteq R' == aRb => aR'b かつ S \subseteq S' == aSb => aS'b とすると,
-- (R, S) \subseteq (R', S') == (c, d) (R, S) (a, b) => (c, d) (R', S') (a, b) である.
-- なぜなら左辺は R \subseteq R' かつ S \subseteq S' であり,
-- 右辺は cRa => cR'a かつ dSb => dS'b であるから.
--
-- 交わりの普遍性は任意の (X, Y) について,
-- (X, Y) \subseteq ((R, R') \cap (S, S')) == (X, Y) \subseteq (R, R') /\ (X, Y) \subseteq (S, S') とポイントワイズに定義すれば良い.
--
-- 逆は (R, S)^op = (R^op, S^op) であり,
-- ((R, S)^op)^op = (R^op, S^op)^op = ((R^op)^op, (S^op)^op) = (R, S) となり,
-- 対合が成り立つ
-- また, (R, S) \subseteq (R', S') なら (R, S)^op \subseteq (R', S')^op も言える.
-- なぜなら (R^op, S^op) \subseteq (R'^op, S'^op) であり, ポイントワイズに
-- R^op \subseteq R'^op と S^op \subseteq S'^op も言えるからだ.
-- 反変については ((R, R') . (S, S'))^op = (S, S')^op . (R, R')^op もそのまま成り立つ.
-- 

-- | Ex 4.8
--
-- C : A <- A を余反射とする.
-- つまり C \subseteq id_A とする.
--
--  C . C
-- \subseteq {- 余反射 -}
--  id_A . C
-- = {- 恒等射は単位元 -}
--  C
--

-- | Ex 4.9
--
-- A と B が余反射で A . B と合成できたり A \cap B で交わりが取れるためには同じ型である必要がある.
-- つまり A, B : C <- C であるとする.
-- A \subseteq id_C かつ B \subseteq id_C である.
--
-- 左から右
--  A . B
-- = {- 交わりの冪等 -}
--  A . B \cap A . B
-- \subseteq {- A B 余反射 -}
--  A . id_C \cap id_C . B
-- = {- 恒等射 -}
--  A \cap B
--
-- 右から左
--  A \cap B
-- = {- 恒等射 -}
--  (A . id_C) \cap B
-- \subseteq {- モジュラ則 -}
--  A . (id_C \cap (A^op . B))
-- \subseteq {- 単調性 -}
--  A . id_C \cap A . A^op . B
-- \subseteq {- 交わりの下界 X \cap R \subseteq R -}
--  A . A^op . B
-- \subseteq {- 後述 -}
--  A . (id_C)^op . B
-- = {- (id_C)^op = id_C -}
--  A . id_C . B
-- = {- 恒等射は単位元 -}
--  A . B
--
-- 上記の後述.
--   A が余反射なら A^op も余反射である.以下にそれを論証する.
--
--  Aが余反射
-- = {- 余反射の定義 -}
--  A \subseteq id
-- = {- 逆の順序保存 (4.3) -}
--  A^op \subseteq id^op
-- = {- 恒等射の逆は恒等射 -}
--  A^op \subseteq id
-- = {- 余反射の定義 -}
--  A^op は余反射
--

-- | Ex 4.10
--
-- C を余反射とする.
-- (C . R) \cap S = C . (R \cap S) を示す.
--
--  (C . R) \cap S
-- \subseteq {- モジュラ則 -}
--  C . (R \cap (C^op . S))
-- \subseteq {- C^op も余反射 C^op \subseteq id -}
--  C . (R \cap S)
--
--  C . (R \cap S)
-- \subseteq {- 単調性 -}
--  (C . R) \cap (C . S)
-- \subseteq {- C は余反射 -}
--  (C . R) \cap S
--

-- | Ex 4.11
--
-- Ex 4.9 A, B が余反射なら A . B = A \cap B
-- Ex 4.10 の双対により (X \cap id) . C = (X . C) \cap id
--
-- 下から順に
--
--  (X \cap id) . C   -- 下から一番目
-- = {- Ex 4.9 (X \cap id) は余反射 -}
--  (X \cap id) \cap C
-- = {- 交わりの交換則 -}
--  C \cap (X \cap id)
-- = {- Ex 4.9 -}
--  C . (X \cap id)   -- 下から二番目
-- = {- Ex 4.9 -}
--  C \cap (X \cap id) -- ★途中
-- = {- 交わりの冪等則 -}
--  C \cap (X \cap id) \cap C
-- = {- Ex 4.10 の双対 -}
--  C \cap (X . C \cap id)
-- = {- Ex 4.9 -}
--  C . (X . C \cap id)
-- = {- Ex 4.10 -}
--  (C . X . C) \cap id -- 下から三番目
--
--  (C . X) \cap id  -- 一番上の左辺
-- = {- Ex 4.10 -}
--  C . (X \cap id)  -- 下から二番目
-- = {- Ex 4.9 (X \cap id) は余反射 -}
--  C \cap (X \cap id)
-- = {- 交わりの交換則 -}
--  (X \cap id) \cap C
-- = {- Ex 4.10 の双対 -}
--  (X . C) \cap id  -- 一番上の右辺
--

-- | Ex 4.12
--
--  C が余反射なら ran (C . R) = C . ran R を示す.
--
--  ran (C . R)
-- = {- ran の定義 (4.12) ran R = (R . R^op) \cap id -}
--  id \cap (C . R . (C . R)^op)
-- = {- 逆 -}
--  id \cap (C . R . R^op . C^op)
-- = {- 余反射は対称的 -}
--  id \cap (C . R . R^op . C)
-- = {- Ex 4.11 (C . X . C) \cap id = C . (X \cap id) -}
--  C . (id \cap (R . R^op))
-- = {- ran の定義 (4.12) ran R = (R . R^op) \cap id -}
--  C . ran R
--

-- | Ex 4.13
--
-- R . R = R なら冪等.
-- 対称的かつ推移的なら冪等であることを示す.
--
-- 対称的 : R \subseteq R^op これは R = R^op でもある.
-- 推移的 : R . R \subseteq R
--
--
--  R
-- \subseteq {- Ex 4.6 -}
--  R . R^op . R
-- = {- R が対称的 -}
--  R . R . R
-- \subseteq {- R が推移的 -}
--  R . R
--

-- | Ex 4.14
--
-- R = R . R^op <=> R が対称的かつ推移的
--
-- (<=)
-- Ex 4.13 より対称的かつ推移的なら冪等である.
-- R . R = R だが 対称的なら R = R^op でもあるので R . R^op = R が言える.
--
-- (=>)
--
