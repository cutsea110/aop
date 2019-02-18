module OpenSet where

import Data.List as List ((\\))
import Data.Set as Set

dup x = (x, x)
cross (f, g) (x, y) = (f x, g y)
pair (f, g) x = (f x, g x)

openSets :: Ord a => [a] -> [[[a]]]
openSets x = toList $ Set.map (toList.(Set.map toList)) $ Set.filter isOpen candidates
    where
        (e, u) = (fromList [], fromList x)
        conpact = fromList [e, u]
        pu = powerSet u
        candidates = Set.map (union conpact) $ powerSet (pu Set.\\ conpact)
        sub = Prelude.map toList $ toList (pu Set.\\ conpact)
        isOpen o = Set.fold (\a b -> p a && b) True ps
            where
                p (a, b) = uncurry (&&) (intersection a b `member` o, union a b `member` o)
                ps = Set.filter (uncurry (<)) $ uncurry cartesianProduct $ dup (o Set.\\ conpact)
    
main :: IO ()    
main = mapM_ go $ zip [1..] $ openSets [0,1,2]
    where
        go (i, ln) = putStrLn $ show i ++ " : " ++ show ln
