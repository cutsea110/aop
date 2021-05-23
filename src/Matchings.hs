module Matchings (match) where

import Expressions
import Substitutions (Subst, unitSub, combine)
import Utilities (parts)

alignments (Compose as, Compose bs)
  = [ zip as (map Compose bss) | bss <- parts n bs ]
  where
    n = length as

matchA :: (Atom, Expr) -> [Subst]
matchA (Var v, e) = [unitSub v e]
matchA (Con k1 es1, Compose [Con k2 es2])
  | k1 == k2 = combine (map match (zip es1 es2))
matchA _ = []

match :: (Expr, Expr) -> [Subst]
match = concatMap (combine . map matchA) . alignments
