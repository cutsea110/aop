{-# LANGUAGE  DeriveFunctor #-}
module TuplingAsHist where

import Prelude hiding (foldr, unfoldr, subtract)
import Debug.Trace (trace)

f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}"
         in trace msg v

basef :: t -> (a -> b -> t) -> BaseF a b -> t
basef c f NilF = c
basef c f (ConsF x xs) = f x xs

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
unfoldr :: (b -> BaseF a b) -> b -> List a
unfoldr psi = v
  where v x = case psi x of
          NilF       -> Nil
          ConsF x xs -> Cons x (v xs)

out :: List a -> BaseF a (List a)
out Nil         = NilF
out (Cons x xs) = ConsF x xs

data Tupling t a = Unit t
                 | Node (t, a) (Tupling t a)
                 deriving Show

-- | Histomorphism
--                             inF
--          uF <--------------------------------------------- F(uF)
--          /|                                                  |
--         / |                                                  |
--  histo /  | u = cata (inF* . <phi, id>)                      | Fu
--       /   |                                                  |
--      /    |                                                  |
--     v     v                                                  v
--    A <--- vF* <-------- F*(vF*) == A * F(uF*) <----------- F(vF*)
--        e         inF*                             <phi, id>
--
histo :: (BaseF a (Tupling t a) ->  t) -> List a -> t
histo phi = extract . u
  where
    u x = case m of
      NilF       -> Unit t
      ConsF x xs -> Node (t, x) xs
      where
        m = fmap u (out x)
        t = phi m
      

extract :: Tupling t a -> t
extract (Unit t)        = t
extract (Node (t, _) x) = t

subtract :: Tupling t a -> BaseF a (Tupling t a)
subtract (Unit _)        = NilF
subtract (Node (_, a) x) = ConsF a x

dyna :: (BaseF a (Tupling t a) -> t) -> (b -> BaseF a b) -> b -> t
dyna f g = histo f . unfoldr g

phi :: BaseF Int (Tupling Int Int) -> Int
phi NilF = 1
phi (ConsF a x) = a + extract x

psi :: Int -> BaseF Int Int
psi n = if n == 0 then NilF else ConsF n (n-1)

sumTo n = dyna (phi $?) psi n
