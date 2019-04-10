module ChurchEncoding where

import Prelude hiding (head, tail)

bool p a b = p a b
false a b = b
true a b = a

cNot p a b = p b a
cAnd p q = undefined -- p q p -- couldn't infer type
cOr p q = undefined -- p p q -- couldn't infer type
cXor p q = p (cNot q) q

testBool1 = bool true 1 2
testBool2 = bool false 1 2

pair (x1, x2) = \s -> s (x1, x2)
first p = p (\(x1, x2) -> x1)
second p = p (\(x1, x2) -> x2)

-- ref.) https://en.wikipedia.org/wiki/Church_encoding
zero f x = x
one  f x = f x
two f x = f (f x)

suc n f x = f (n f x)
plus m n f x = m f (n f x)
mult m n f x = m (n f) x
expr m n f x = (n m) f x

cons = pair
head = first
tail = second
nil = false
isnil l = l (\h t d -> false) true
