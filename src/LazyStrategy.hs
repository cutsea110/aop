module LazyStrategy where
-- | ref.) https://zenn.dev/nobsun/scraps/50035a21debd35

import Data.List (unfoldr)

_null :: [a] -> String
_null []    = "empty"
_null (_:_) = "not empty"


{- |
ghci> :sprint _as
_as = _
ghci> _null _as
"not empty"
ghci> :sprint _as
_as = _ : _
ghci> let a3 =_as !! 3
ghci> :sprint a3
a3 = _
ghci> a3
4
ghci> :sprint _as
_as = _ : _ : _ : 4 : _
ghci> length _as
7
ghci> :sprint _as
_as = [_,_,_,4,_,_,_]
ghci> sum _as
28
ghci> :sprint _as
_as = [1,2,3,4,5,6,7]
-}
_as :: [Int]
_as = [n+0|n <- [1..7]]

{- |
ghci> :sprint _bs
_bs = _
ghci> _null _bs
"not empty"
ghci> :sprint _bs
_bs = [_,_,_,_,_,_,_]
ghci> let b3 = _bs !! 3
ghci> :sprint b3
b3 = _
ghci> b3
4
ghci> :sprint _bs
_bs = [_,_,_,4,_,_,_]
ghci> length _bs
7
ghci> :sprint _bs
_bs = [_,_,_,4,_,_,_]
ghci> sum _bs
28
ghci> :sprint _bs
_bs = [1,2,3,4,5,6,7]
-}
_bs :: [Int]
_bs = [1+0,2+0,3+0,4+0,5+0,6+0,7+0]

{- |
ghci> :sprint _bs'
_bs' = _
ghci> _null _bs'
"not empty"
ghci> :sprint _bs'
_bs' = [1,_,3,_,5,_,7]
ghci> let b3 = _bs' !! 3
ghci> :sprint b3
b3 = _
ghci> b3
4
ghci> :sprint _bs'
_bs' = [1,_,3,4,5,_,7]
ghci> length _bs'
7
ghci> :sprint _bs'
_bs' = [1,_,3,4,5,_,7]
ghci> sum _bs'
28
ghci> :sprint _bs'
_bs' = [1,2,3,4,5,6,7]
-}
_bs' :: [Int]
_bs' = [1, 2+0, 3, 4+0, 5, 6+0, 7]

{- |
ghci> :sprint _bs''
_bs'' = _
ghci> _null _bs''
"not empty"
ghci> :sprint _bs''
_bs'' = _ : _
ghci> _bs'' !! 1
2
ghci> :sprint _bs''
_bs'' = _ : 2 : _
ghci> _bs'' !! 2
3
ghci> :sprint _bs''
_bs'' = _ : 2 : 3 : _
ghci> _bs'' !! 3
4
ghci> :sprint _bs''
_bs'' = [_,2,3,4,_,_,_]
ghci> length _bs''
7
ghci> :sprint _bs''
_bs'' = [_,2,3,4,_,_,_]
ghci> sum _bs''
28
ghci> :sprint _bs''
_bs'' = [1,2,3,4,5,6,7]
-}
_bs'' :: [Int]
_bs'' = let xs = [4+0,5+0,6+0,7+0] in (1+0):(2+0):(3+0):xs

{- |
ghci> :sprint _cs
_cs = _
ghci> _null _cs
"not empty"
ghci> :sprint _cs
_cs = [1,2,3,4,5,6,7]
ghci> let c3 = _cs !! 3
ghci> :sprint c3
c3 = _
ghci> c3
4
ghci> :sprint _cs
_cs = [1,2,3,4,5,6,7]
ghci> length _cs
7
ghci> :sprint _cs
_cs = [1,2,3,4,5,6,7]
ghci> sum _cs
28
ghci> :sprint _cs
_cs = [1,2,3,4,5,6,7]
-}
_cs :: [Int]
_cs = [1,2,3,4,5,6,7]

{- |
ghci> :sprint _ds
_ds = _
ghci> _null _ds
"not empty"
ghci> :sprint _ds
_ds = _
ghci> let d3 = _ds !! 3
ghci> :sprint d3
d3 = _
ghci> d3
4
ghci> :sprint _ds
_ds = _
ghci> length _ds
7
ghci> :sprint _ds
_ds = _
ghci> sum _ds
28
ghci> :sprint _ds
_ds = _
-}
_ds :: (Enum a, Num a) => [a]
_ds = [1..7]
