-- | ref.) https://www.ipsj.or.jp/07editj/promenade/4605.pdf
module TicketProblem where

data DumbTree = Empty | Fork DumbTree DumbTree
instance Show DumbTree where
  show Empty = "0"
  show (Fork Empty Empty) = "(" ++ "L" ++ "^" ++ "R" ++ ")"
  show (Fork Empty r) = "(" ++ "L" ++ "^" ++ show r ++ ")"
  show (Fork l Empty) = "(" ++ show l ++ "^" ++ "R" ++ ")"
  show (Fork l r) = "(" ++ show l ++ "^" ++ show r ++ ")"

-- trees 整数(>0)から, DumbTree のリストへ
trees :: Int -> [DumbTree]
trees 1 = [Empty]
trees n = concat [ joins ls rs
                 | (ls, rs) <- [ lrs xs ys
                               | (xs, ys) <- splits1 n
                               ]
                 ]
splits1 :: Int -> [(Int, Int)]
splits1 1 = []
splits1 n = (1, n-1):[ (i+1, j)
                     | (i, j) <- splits1 (n-1)
                     ]

lrs :: Int -> Int -> ([DumbTree], [DumbTree])
lrs xs ys = (trees xs, trees ys)

joins :: [DumbTree] -> [DumbTree] -> [DumbTree]
joins ls rs = [ Fork l r | l <- ls, r <- rs ]
