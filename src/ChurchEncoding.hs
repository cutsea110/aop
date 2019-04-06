module ChurchEncoding where

bool = ($)
false = fst
true = snd

testBool1 = bool true (1,2)
testBool2 = bool false (1,2)

pair (x1, x2) = \s -> s (x1, x2)
first p = p (\(x1, x2) -> x1)
second p = p (\(x1, x2) -> x2)
