module Prime where

multiples = [map (n*) [1..] | n <- [2..]]

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y  = x : merge xs (y:ys)
                    | x == y = x : merge xs ys
                    | x > y  = y : merge (x:xs) ys

xmerge (x:xs) ys = x : merge xs ys
mergeAll (xs:xss) = xmerge xs (mergeAll xss)

primes = 2 : ([3..] \\ composites)
  where
    composites = mergeAll [map (p*) [p..] | p <- primes]

(x:xs) \\ (y:ys) | x < y  = x : (xs \\ (y:ys))
                 | x == y = xs \\ ys
                 | x > y  = (x:xs) \\ ys
