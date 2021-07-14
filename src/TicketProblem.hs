-- | ref.) https://www.ipsj.or.jp/07editj/promenade/4605.pdf
module TicketProblem where
{--
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
--}

data Term = Val Char | App Char Term Term

trees :: [Char] -> [Char] -> [Term]
trees ds os = [ t | (_, t) <- [ otree os u | u <- dtrees ds ]]

dtrees :: [Char] -> [Term]
dtrees [x] = [Val x]
dtrees ds = concat [ joins ls rs | (ls, rs) <- [ lrs xs ys | (xs, ys) <- splits1 ds ]]

splits1 :: [Char] -> [([Char], [Char])]
splits1 [x]    = []
splits1 (x:xs) = ([x], xs) : [ (x:ys, zs) | (ys, zs) <- splits1 xs ]

lrs :: [Char] -> [Char] -> ([Term], [Term])
lrs xs ys = (dtrees xs, dtrees ys)

joins :: [Term] -> [Term] -> [Term]
joins ls rs = [ App '^' l r | l <- ls, r <- rs ]

otree :: [Char] -> Term -> ([Char], Term)
otree os (Val c)     = (os, Val c)
otree os (App _ l r) = (os'', App o' l' r')
  where (o':os', l') = otree os  l
        (os''  , r') = otree os' r

instance Show Term where
  show (Val c) = [c]
  show (App o l r) = "(" ++ show l ++ [o] ++ show r ++ ")"
