module MSS where

subseqs :: [a] -> [[a]]
subseqs = foldr op [[]]
  where op x xss = xss ++ map (x:) xss

mss :: [Integer] -> Integer
mss = foldr op 0 where op x y = y `max` (x + y)
