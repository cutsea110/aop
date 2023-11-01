module Group where

import Control.Monad (forM_)
import Data.List (sortOn, nub, sort)
import Data.Monoid
import Text.Printf (printf)

import Combinatorial (perms)

type DIM = Int
-- | Symmetric group of degree n
data Sym = Sym DIM [Int] deriving (Eq, Show)

syms :: DIM -> [Sym]
syms n = Sym n <$> perms [1..n]

apply :: Sym -> Sym -> Sym
apply (Sym n xs) (Sym n' ys)
  | n == n'   = Sym n $ map ((xs!!) . pred) ys
  | otherwise = error "apply: dimensions do not match" 

test :: [Endo Sym]
test = map (Endo . apply) (syms 3)
test_ :: Sym
test_ = appEndo (test!!3 <> test!!2) $ Sym 3 [1,2,3]

---------------

{--
type S4Op = String

s4 :: [(S4Op, [Int])]
s4 = zip s4Names (perms [1..4])

g :: [[Int]]
g = [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]]

leftCosets, rightCosets :: [[S4Op]]
leftCosets  = nub $ map lCoset s4Names
rightCosets = nub $ map rCoset s4Names

lCoset, rCoset :: S4Op -> [S4Op]
lCoset op = map (opNameOf . (\op' -> apply (compose op  op') [1,2,3,4]) . opNameOf) g
rCoset op = map (opNameOf . (\op' -> apply (compose op'  op) [1,2,3,4]) . opNameOf) g

leftEqualRight :: S4Op -> Bool
leftEqualRight op = sort (lCoset op) == sort (rCoset op)

compose :: S4Op -> S4Op -> S4Op
compose op1 op2 = opNameOf . apply op1 . apply op2 $ [1,2,3,4]

s4Names :: [S4Op]
s4Names = fmap (printf "s%02d") ([0..23] :: [Int])

op :: S4Op -> [Int]
op name = case lookup name s4 of
  Just xs -> xs
  Nothing -> error $ "op: " ++ name ++ " not found"

complement :: S4Op -> S4Op
complement = opNameOf . map fst . sortOn snd . zip [1..] . op

-- | g . f . g'
covariantOver :: S4Op -> S4Op -> [Int] -> [Int]
f `covariantOver` g = apply g . apply f . apply g'
  where
    g' = complement g

opNameOf :: [Int] -> S4Op
opNameOf xs = case lookup xs dict of
  Just name -> name
  Nothing -> error $ "opNameOf: " ++ show xs ++ " not found"
  where
    dict = map swap s4
    swap (x,y) = (y,x)

apply :: S4Op -> [Int] -> [Int]
apply name = map (\i -> op name !! pred i)

table :: IO ()
table = do
  -- header
  putHeader
  -- separator
  putSeparator
  -- lines
  forM_ s4Names $ \l -> do
    printf "%4s |" l
    forM_ s4Names $ \r -> do
      printf "%4s" (compose l r)
    putStrLn ""
  where
    putNewline = putStrLn ""
    
    putHeader = do
      printf "     |"
      forM_ s4Names $ printf "%4s"
      putNewline
      
    putSeparator = do
      putStr "-----+"
      forM_ s4Names $ \_ -> putStr "----"
      putNewline
--}
