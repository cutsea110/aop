module LazyStrategy where
-- | ref.) https://zenn.dev/nobsun/scraps/50035a21debd35

import Data.List (unfoldr)

_null :: [a] -> String
_null []    = "empty"
_null (_:_) = "not empty"



_as :: [Int]
_as = [n+0|n <- [1..7]]

_bs :: [Int]
_bs = let xs = [7,6..1] in [xs!!6, xs!!5, xs!!4, xs!!3, xs!!2, xs!!1, xs!!0]

_cs :: [Int]
_cs = [1,2,3,4,5,6,7]

_ds :: (Enum a, Num a) => [a]
_ds = [1..7]
