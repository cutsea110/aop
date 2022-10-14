module Fact where

-- fact n = if n == 0 then 1 else n * fact (n-1)
factF f = \n -> if n == 0 then 1 else n * f (n-1)

fact0 :: Int -> Int
fact0 = undefined

fact1 = factF fact0
fact2 = factF fact1
fact3 = factF fact2
fact4 = factF fact3
fact5 = factF fact4

-- fact = factF fact
fix f = let g = f g in g
fact = fix factF
