{-# LANGUAGE BangPatterns,NPlusKPatterns #-}
module Frog where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

parseInt = C.readInt . C.dropWhile isSpace
getIntVec n = V.unfoldrN n parseInt <$> C.getLine

data T = T T {-# UNPACK #-}!Int T
tmap f (T l x r) = T (tmap f l) (f x) (tmap f r)

index :: T -> Int -> Int
index (T _ x _) 0 = x
index (T l _ r) (n+1) = case n `divMod` 2 of
  (q, 0) -> index l q
  (q, 1) -> index r q

nats :: T
nats = go 0 1 where
  go !n !s = T (go l s') n (go r s') where
    l = n+s
    r = l+s
    s'= s*2

main = do
  n <- readLn :: IO Int
  hs <- getIntVec n
  let f_tree = tmap (f fastest_f) nats
        where fastest_f = index f_tree
              x#y = abs $ hs V.! x - hs V.! y
              f mf 0 = 0
              f mf 1 = 1#0
              f mf i = sub i 1 `min` sub i 2
                where sub m j = mf (m-j) + m#(m-j)
      
  print $ index f_tree (n-1)
