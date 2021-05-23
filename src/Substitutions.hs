module Substitutions (Subst, emptySub, unitSub, combine, apply) where

import Expressions
import Utilities (cp)
import Data.Maybe (fromJust)


type Subst = [(VarName, Expr)]

emptySub = []

unitSub v e = [(v, e)]

apply :: Subst -> Expr -> Expr
apply sub (Compose as)
  = Compose (concatMap (applyA sub) as)

applyA sub (Var v) = deCompose (bindings sub v)
applyA sub (Con k es) = [Con k (map (apply sub) es)]

combine = concatMap unifyAll . cp

unify :: Subst -> Subst -> [Subst]
unify sub1 sub2 = if compatible sub1 sub2
                  then [union sub1 sub2]
                  else []

compatible [] sub2 = True
compatible sub1 [] = True
compatible sub1@((v1, e1):sub1') sub2@((v2, e2):sub2')
  | v1 < v2 = compatible sub1' sub2
  | v1 == v2 = if e1 == e2 then compatible sub1' sub2' else False
  | v1 > v2 = compatible sub1 sub2'

union [] sub2 = sub2
union sub1 [] = sub1
union sub1@((v1, e1):sub1') sub2@((v2, e2):sub2')
  | v1 < v2 = (v1, e1):union sub1' sub2
  | v1 == v2 = (v1, e1):union sub1' sub2'
  | v1 > v2 = (v2, e2):union sub1 sub2'

unifyAll :: [Subst] -> [Subst]
unifyAll = foldr f [emptySub]
  where
    f sub subs = concatMap (unify sub) subs


bindings :: Subst -> VarName -> Expr
bindings sub v = fromJust (lookup v sub)
