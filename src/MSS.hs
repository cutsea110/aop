module MSS where

cpl (xs, y) = [(x, y) | x <- xs]
cpr (x, ys) = [(x, y) | y <- ys]
outl = fst
outr = snd
nil = []
cons (x, xs) = x:xs
tau x = [x]
union = concat
plus (x, y) = x + y

subseqs :: [a] -> [[a]]
subseqs = foldr op [[]]
  where op x xss = xss ++ map (x:) xss

inits :: [a] -> [[a]]
inits = foldr op c
  where c = tau nil
        op x xs = c ++ map cons (cpr (x, xs))

tails :: [a] -> [[a]]
tails = foldr extend (tau nil)
  where extend x yys@(y:ys) = (x:y):yys

-- Algebra of Programming: Exercise 7.40

mss0 :: [Integer] -> Integer
mss0 = maximum . map sum . segments
  where segments = union . map inits . tails

mss1 :: [Integer] -> Integer
mss1 = maximum . map sumOfPrefix . tails
  where sumOfPrefix = foldr k c
          where c = 0
                k x y = max 0 (x+y)

mss2 :: [Integer] -> Integer
mss2 = maximum . map (maximum . g) . tails
  where g = foldr f c
          where c = [0]
                f x y = map plus (cpr (x, y)) ++ c

mss3 :: [Integer] -> Integer
mss3 = maximum . map g . tails
  where g = foldr oplus zero
          where zero = 0
                oplus x y = maximum $ tau 0 ++ tau (plus (x, y))

mss4 :: [Integer] -> Integer
mss4 = maximum . foldr h (tau 0)
  where h x yys@(y:ys) = maximum (tau 0 ++ [x+y]) : yys

mss4' :: [Integer] -> Integer
mss4' = maximum . foldr h (tau 0)
  where h x yys@(y:ys) = max 0 (x+y) : yys
