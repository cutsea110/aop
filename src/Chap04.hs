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

