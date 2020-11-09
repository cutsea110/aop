{-# LANGUAGE BangPatterns,NPlusKPatterns #-}
module Frog where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as V

parseInt = C.readInt . C.dropWhile isSpace
getIntVec n = V.unfoldrN n parseInt <$> C.getLine

data Tree a = Tree (Tree a) a (Tree a) deriving Show
instance Functor Tree where
  fmap f (Tree l x r) = Tree (fmap f l) (f x) (fmap f r)
index :: Tree a -> Int -> a
index (Tree _ x _) 0 = x
index (Tree l _ r) (n+1) = case n `divMod` 2 of
  (q, 0) -> index l q
  (q, 1) -> index r q

nats :: Tree Int
nats = go 0 1 where
  go !n !s = Tree (go l s') n (go r s') where
    l = n+s
    r = l+s
    s'= s*2

main = do
  n <- readLn :: IO Int
  hs <- getIntVec n
  let f_tree = fmap (f fastest_f) nats
        where fastest_f = index f_tree
              x#y = abs $ hs V.! x - hs V.! y
              f mf 0 = 0
              f mf 1 = 1#0
              f mf i = sub i 1 `min` sub i 2
                where sub m j = mf (m-j) + m#(m-j)
      
  print $ index f_tree (n-1)
