-- | ref.) http://titech-ssr.blog.jp/archives/1047835805.html
module Fib where

import Prelude hiding (Functor)
import FixPrime

fib :: Integer -> Integer
fib = hylo phi psi
  where
    psi 0 = FZero
    psi 1 = FOne
    psi n = FNode (n-1) (n-2)
    
    phi FZero = 0
    phi FOne  = 1
    phi (FNode x y) = x + y


data FibF a = FZero | FOne | FNode a a

instance Functor FibF where
  fmap _ FZero = FZero
  fmap _ FOne  = FOne
  fmap f (FNode x y) = FNode (f x) (f y)

type Fib = Fix FibF

fib' :: Num t => Fib -> t
fib' = histo phi
  where
    phi :: Num t => FibF (Cofree FibF t) -> t
    phi FZero = 0
    phi FOne  = 1
    phi (FNode f1 f2) = extract f1 + extract f2

toFib 0 = In FZero
toFib 1 = In FOne
toFib n = In (FNode (toFib (n-1)) (toFib (n-2)))

-- | memoized ver
fib_ = (Prelude.map f [0..] !!) where f 0 = 1;f 1 = 1;f n = fib_ (n-2) + fib_ (n-1)
