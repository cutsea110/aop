module Group where

import Control.Monad (forM_)
import Data.List (sortOn, nub, sort)
import Data.Monoid (Endo(..))
import Text.Printf (printf)

import Combinatorial (perms)

type DIM = Int
-- | Symmetric group of degree n
data Sym = Sym DIM [Int] deriving (Eq, Ord, Show)

syms :: DIM -> [Sym]
syms n = Sym n <$> perms [1..n]

apply :: Sym -> Sym -> Sym
apply (Sym n xs) (Sym n' ys)
  | n == n'   = Sym n $ map ((xs!!) . pred) ys
  | otherwise = error "apply: dimensions do not match" 

toEndo :: Sym -> Endo Sym
toEndo = Endo . apply

complement :: Sym -> Sym
complement (Sym n xs) = Sym n xs'
  where xs' = map fst . sortOn snd . zip [1..] $ xs

covariantOver :: Sym -> Sym -> Sym -> Sym
f `covariantOver` g = apply g . apply f . apply (complement g)

g3 :: [Sym]
g3 = map (Sym 3) [[1,2,3],[2,3,1],[3,1,2]]

g4 :: [Sym]
g4 = map (Sym 4) [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]]

(>+) :: Sym -> [Sym] -> [Sym]
f >+ xs = map (f `apply`) xs
(+<) :: [Sym] -> Sym -> [Sym]
xs +< f = map (`apply` f) xs
