module ChurchEncoding where

import Prelude hiding (head, tail, foldr, not, and, or, xor)

bool p a b = p a b
false a b = b
true a b = a

not p a b = p b a
-- ref.) https://github.com/Risto-Stevcev/haskell-church-encodings/blob/master/Rank1Types/Church.hs
and p q = p q false -- p q p -- couldn't infer type
or p q = p true q -- p p q -- couldn't infer type
xor p q = p (not q) q

testBool1 = bool true 1 2
testBool2 = bool false 1 2

pair x1 x2 = \s -> s x1 x2
first p = p (\x1 -> \x2 -> x1)
second p = p (\x1 -> \x2 -> x2)

-- ref.) https://en.wikipedia.org/wiki/Church_encoding
zero f x = x
one  f x = f x
two f x = f (f x)
iszero n = n (\x -> false) true

suc n f x = f (n f x)
plus m n f x = m f (n f x)
mult m n f x = m (n f) x
expr m n f x = (n m) f x

nil = pair true true
isnil = first
cons x xs = pair false (pair x xs)
head z = first (second z)
tail z = second (second z)

-- ref.) http://d.hatena.ne.jp/syamino/20120524/p1
foldr n c l = l c n

