module Matchings (match) where

import Expressions
import Substitutions (Subst, emptySub, unitSub, combine)
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
match = xmatch emptySub

xmatch sub (e1, e2)
  = concat [xmatchesA sub aes | aes <- alignments (e1, e2)]

xmatchesA sub [] = [sub]
xmatchesA sub (ae:aes)
  = concat [xmatchesA sub' aes | sub' <- xmatchA sub ae]

xmatchA sub (Var v, e) = extend sub v e
xmatchA sub (Con k1 es1, Compose [Con k2 es2])
  | k1 == k2 = xmatches sub (zip es1 es2)
xmatchA _ _ = []

xmatches sub [] = [sub]
xmatches sub ((e1, e2):es)
  = concat [xmatches sub' es | sub' <- xmatch sub (e1, e2)]

extend sub v e
  = case lookup v sub of
      Nothing -> [(v, e):sub]
      Just e' -> if e == e' then [sub]
                 else []
