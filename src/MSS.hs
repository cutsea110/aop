module MSS where

subseqs :: [a] -> [[a]]
subseqs = foldr op [[]]
  where op x xss = xss ++ map (x:) xss

inits :: [a] -> [[a]]
inits = foldr op [[]]
  where op x xss = [] : map (x:) xss

tails :: [a] -> [[a]]
tails = foldr op [[]]
  where op x xss = (x : head xss) : xss

-- Algebra of Programming: Exercise 7.40

mss1 :: [Integer] -> Integer
mss1 = maximum . map sum . segments
  where segments = concatMap inits . tails

mss2 :: [Integer] -> Integer
mss2 = maximum . map (maximum . f) . tails
  where f = foldr op c
          where c = [0]
                op x y = map (x+) y ++ c

mss3 :: [Integer] -> Integer
mss3 = maximum . map f . tails
  where f = foldr oplus zero
          where zero = 0
                oplus x y = max (x+y) zero

mss4 :: [Integer] -> Integer
mss4 = maximum . foldr op [zero]
  where zero = 0
        op x y = max (x + head y) zero : y
