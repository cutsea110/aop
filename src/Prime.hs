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
--
-- g = (\f n -> if n == 0 then 1 else n * f (n-1)) fact0
--
-- fact0 n = undefined
--    n = 0 => undefined
--
-- fact1 = (\f n -> if n == 0 then 1 else n * f (n-1)) fact0
-- fact1 n = if n == 0 then 1 else n * fact0 (n-1)
--    n = 0 => 1
--    n = 1 => n * fact0 0 => n * undefined => undefined
--
-- fact2 = (\f n -> if n == 0 then 1 else n * f (n-1)) fact1
-- fact2 n = if n == 0 then 1 else n * fact1 (n-1)
--    n = 0 => 1
--    n = 1 => 1 * fact1 0 => 1 * 1 => 1
--    n = 2 => 2 * fact1 1 => 2 * undefined => undefined
--
-- fact3 = (\f n -> if n == 0 then 1 else n * f (n-1)) fact2
-- fact3 n = if n == 0 then 1 else n * fact2 (n-1)
--    n = 0 => 1
--    n = 1 => 1 * fact2 0 => 1 * 1 => 1
--    n = 2 => 2 * fact2 1 => 2 * 1 => 2
--    n = 3 => 3 * fact2 2 => 3 * undefined => undefined
--
-- fact4 = (\f n -> if n == 0 then 1 else n * f (n-1)) fact3
-- fact4 n = if n == 0 then 1 else n * fact3 (n-1)
--    n = 0 => 1
--    n = 1 => 1 * fact3 0 => 1 * 1 => 1
--    n = 2 => 2 * fact3 1 => 2 * 1 => 2
--    n = 3 => 3 * fact3 2 => 3 * 2 => 6
--    n = 4 => 4 * fact3 3 => 4 * undefined => undefined
--
-- ....
--
-- fact = (\f n -> if n == 0 then 1 else n * f (n-1)) fact
-- fact n = if n == 0 then 1 else n * fact (n-1)
--    n = 0 => 1
--    n = 1 => 1 * fact 0 => 1 * 1 => 1  -- 一つ上から
--    n = 2 => 2 * fact 1 => 2 * 1 => 2  -- 一つ上から
--    n = 3 => 3 * fact 2 => 3 * 2 => 6  -- 一つ上から
--    n = 4 => 4 * fact 3 => 4 * 6 => 24 -- 一つ上から
--    :
--    :
--
