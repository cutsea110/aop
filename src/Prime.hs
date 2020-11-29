module Prime where

import Prelude hiding (elem)

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

prs n = approx n (2 : ([3..] \\ crs n))
crs n = mergeAll [map (p*) [p..] | p <- prs n]

approx n [] = []
approx n (x:xs) | n > 0 = x : approx (n-1) xs

-- prs n = p1:p2..pn:undef であるとき
-- crs n = c1:c2..cm:undef where cm=pn^2 となることを示せ.

-- crs 1 = c1:undef where c1 = p1^2 を示す
-- crs n = c1:c2..cm:undef where cm = pn^2 が成り立つとする.
-- crs (n+1) = c1:c2..cm:undef cm = p(n+1)^2 を証明する.

-- prs 1 = approx 1 (2 : ([3..] \\ crs 1))
--       = 2 : approx 0 ([3..] \\ crs 1)
--       = 2 : undefined
-- crs 1 = 2^2 : undefined
--
-- crs n = c1:c2..cm:undef where cm=pn^2 と仮定
-- crs (n+1) = mergeAll [map (p*) [p..] | p <- prs (n+1)]
--           = mergeAll [map (p*) [p..] | p <- p1:p2..p(n+1):undef]
--           = mergeAll ((map (p1*) [p1..]):
--                       (map (p2*) [p2..]):
--                       ..
--                       (map (pn*) [pn..]):
--                       (map (p(n+1)*) [p(n+1)..]):
--                       undef)
--           = xmerge (map (p1*) [p1..])
--                    (mergeAll ((map (p2*) [p2..]):
--                               ..
--                               (map (pn*) [pn..]):
--                               (map (p(n+1)*) [p(n+1)..]):
--                               undef))
--           = xmerge (map (p1*) [p1..])
--                    (xmerge (map (p2*) [p2..])
--                            mergeAll (..:(map (pn*) [pn..]):(map (p(n+1)*) [p(n+1)..]):undef))
--           = xmerge (map (p1*) [p1..])
--                    (xmerge (map (p2*) [p2..])
--                            ..
--                            xmerge (map (pn*) [pn..])
--                                   (mergeAll (map (p(n+1)*) [p(n+1)..]) undef)
--           = xmerge (map (p1*) [p1..])
--                    (xmerge (map (p2*) [p2..])
--                            ..
--                            xmerge (map (pn*) [pn..])
--                                   (p(n+1)*p(n+1) : (merge (map (p(n+1)*) [p(n+1)+1,..]) undef)
--           = xmerge (map (p1*) [p1..])
--                    (xmerge (map (p2*) [p2..])
--                            ..
--                            xmerge (map (pn*) [pn..])
--                                   (p(n+1)*p(n+1) : undef)
--           = xmerge (map (p1*) [p1..])
--                    (xmerge (map (p2*) [p2..])
--                            ..
--                            xmerge (map (pn*) [pn..])
--                                   (p(n+1)*p(n+1) : undef)


primes' = 2 : [2 * n + 1 | n <- [1..] \\ sundaram]
sundaram = mergeAll [[ i + j + 2 * i * j | j <- [i..]] | i <- [1..]]

data Torus a = Cell a (Torus a) (Torus a) (Torus a) (Torus a)
elem (Cell a u d l r) = a
up (Cell a u d l r) = u
down (Cell a u d l r) = d
left (Cell a u d l r) = l
right (Cell a u d l r) = r

instance Show a => Show (Torus a) where
  show x = show (elem x)

type Matrix a = [[a]]

mkTorus :: Matrix a -> Torus a
mkTorus ass = head (head xss)
  where
    xss = zipWith5 (zipWith5 Cell) ass (rotr xss) (rotl xss) (map rotr xss) (map rotl xss)

rotr xs = [last xs] ++ init xs
rotl xs = tail xs ++ [head xs]

-- zipWith5 f (x:xs) ~(y:ys) ~(z:zs) ~(v:vs) ~(w:ws) = f x y z v w:zipWith5 f xs ys zs vs ws
zipWith5 f (x:xs) ~(y:ys) ~(z:zs) ~(v:vs) ~(w:ws) = f x y z v w:zipWith5 f xs ys zs vs ws
zipWith5 _ _ _ _ _ _ = []
