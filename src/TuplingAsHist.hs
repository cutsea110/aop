{-# LANGUAGE  DeriveFunctor #-}
module TuplingAsHist where

import Prelude hiding (foldr, unfoldr)


-- base functor
data BaseF a x = NilF
               | ConsF a x
               deriving (Show, Functor)


-- List a is fixpoint of BaseF
data List a = Nil
            | Cons a (List a)
            deriving (Show, Functor)

--               [nil, cons]
--   List a <------------------ 1 + a * List a
--      |                         |
--    u |                         | id + id_a * u
--      v                         v
--      X   <------------------ 1 + a * X
--                 [c, f]
--
foldr :: b -> (a -> b -> b) -> List a -> b
foldr c f = u
  where u Nil = c
        u (Cons x xs) = f x (u xs)

--               [nil, cons]
--          <------------------
--   List a ------------------> 1 + a * List a
--      ^          out            ^
--    v |                         | id + id_a * v
--      |                         |
--      X   ------------------> 1 + a * X
--                 psi
--
unfoldr :: (b -> Maybe (a, b)) -> b -> List a
unfoldr psi = v
  where v x = case psi x of
          Nothing -> Nil
          Just (x, xs) -> Cons x (v xs)
