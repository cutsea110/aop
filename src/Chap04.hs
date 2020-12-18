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
