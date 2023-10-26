module Group where

import Combinatorial (perms)

type S4Op = String

toName :: Int -> S4Op
toName n = "s" ++ show n

s4 :: [(S4Op, [Int])]
s4 = zip s4Names (perms [1..4])

s4Names :: [S4Op]
s4Names = fmap toName [0..23]

op :: S4Op -> [Int]
op name = case lookup name s4 of
  Just xs -> xs
  Nothing -> error $ "op: " ++ name ++ " not found"

opNameOf :: [Int] -> S4Op
opNameOf xs = case lookup xs dict of
  Just name -> name
  Nothing -> error $ "opNameOf: " ++ show xs ++ " not found"
  where
    dict = map swap s4
    swap (x,y) = (y,x)

apply :: S4Op -> [Int] -> [Int]
apply name is = map (\i -> op name !! pred i) is

table :: [(S4Op, S4Op, S4Op)]
table = [(l, r, opNameOf o)| l <- s4Names, r <- s4Names, let o = apply l $ apply r [1,2,3,4]]

