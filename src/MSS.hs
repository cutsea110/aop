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
  where f = foldr op [0]
          where op x y = map (x+) y ++ [0]

mss3 :: [Integer] -> Integer
mss3 = maximum . map f . tails
  where f = foldr oplus 0
          where oplus x y = max (x+y) 0

mss4 :: [Integer] -> Integer
mss4 = maximum . foldr op [0]
  where op x y = max (x + head y) 0 : y
