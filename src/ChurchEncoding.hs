module ChurchEncoding where

bool = ($)
false = fst
true = snd

testBool1 = bool true (1,2)
testBool2 = bool false (1,2)
