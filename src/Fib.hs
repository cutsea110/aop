-- | ref.) http://titech-ssr.blog.jp/archives/1047835805.html
module Fib where

import Fix

data FibT a = FZero | FOne | FNode a a

instance Functor FibT where
  fmap _ FZero = FZero
  fmap _ FOne  = FOne
  fmap f (FNode x y) = FNode (f x) (f y)

fib :: Integer -> Integer
fib = hylo phi psi
  where
    psi 0 = FZero
    psi 1 = FOne
    psi n = FNode (n-1) (n-2)
    
    phi FZero = 0
    phi FOne  = 1
    phi (FNode x y) = x + y
