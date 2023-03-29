module Dyna where

--             a = In
--     uF <-------------- F(uF)
--      |                  |
-- cata |                  | F cata
--      v                  v
--      X <-------------- F(X)
--               phi

--              out
--     vF --------------> F(vF)
--      ^                  ^
--  ana |                  | F ana
--      |                  |
--      X --------------> F(X)
--               psi

--             a = In
--     uF <-------------- F(uF)
--      |                  |
-- para |                  | F (id * cata)
--      v                  v
--      X <-------------- F(uF * X)
--               phi

--              out
--     vF --------------> F(vF)
--      ^                  ^
--  apo |                  | F (id + apo)
--      |                  |
--      X --------------> F(vF + X)
--               psi

--                   a = In
--           uF <---------------------- F(uF)
--          / |            A  <-----+    |
-- hist phi/  | cata       ^   phi   \   | F cata
--        /   v            |          \  v
--       A <- vF* <--- A * F(vF*) <---- F(vF*)
--          e     In*      | <phi,id> /
--                         v         /
--                      F(vF*) <----+
--                               id
--
-- F*(X) = A * F(X)
-- vF* = A * F(vF*)
-- ex.)
--   F  == Maybe ==> F(X) = 1 + X
--  uF  == Nat  Z, S Z, S (S Z) ...
--  F*  == A * Maybe ==> F*(X) = A * F(X) = A * (1 + X) == A + A * X
--  vF* == NonEmptyList A
