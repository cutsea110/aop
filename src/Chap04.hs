module Chap04 where

-- | Ex 4.1
--
--  X \subseq R \cap (S \cap T)
-- = {- 交わりの普遍性 -}
--  X \subseq R /\ X \subseq (S \cap T)
-- = {- 交わりの普遍性 -}
--  X \subseq R /\ (X \subseq S /\ X \subseq T)
-- = {- 連言は結合的 -}
--  (X \subseq R /\ X \subseq S) /\ X \subseq T
-- = {- 交わりの普遍性 -}
--  X \subseq (R \cap S) /\ X \subseq T
-- = {- 交わりの普遍性 -}
--  X \subseq (R \cap S) \cap T
