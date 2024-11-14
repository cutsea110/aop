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
  where c = [[]]
        op x xs = c ++ map cons (cpr (x, xs))

tails :: [a] -> [[a]]
tails = foldr op [[]]
  where op x xss = (x : head xss) : xss

-- Algebra of Programming: Exercise 7.40

mss1 :: [Integer] -> Integer
mss1 = maximum . map sum . segments
  where segments = union . map inits . tails

mss2 :: [Integer] -> Integer
mss2 = maximum . map (maximum . g) . tails
  where g = foldr f c
          where c = [0]
                f x y = map plus (cpr (x, y)) ++ c

sumOfPrefix :: [Integer] -> Integer
sumOfPrefix = foldr k c
  where c = 0
        k x y = max 0 (x+y)

mss3 :: [Integer] -> Integer
mss3 = maximum . map g . tails
  where g = foldr oplus zero
          where zero = 0
                oplus x y = max (x+y) zero

mss4 :: [Integer] -> Integer
mss4 = maximum . foldr f [zero]
  where zero = 0
        f x y = max (x + head y) zero : y
