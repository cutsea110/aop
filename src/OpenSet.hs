module OpenSet where

import Data.List as List ((\\))
import Data.Set as Set


openSets :: Ord a => [a] -> [[[a]]]
openSets x = toList $ Set.map (toList.(Set.map toList)) $ Set.filter isOpen candidates
    where
        (e, u) = (fromList [], fromList x)
        conpact = fromList [e, u]
        pu = powerSet u
        candidates = Set.map (union conpact) $ powerSet (pu Set.\\ conpact)
        sub = Prelude.map toList $ toList (pu Set.\\ conpact)
        isOpen o = all (==True) ok
            where
                o' = o Set.\\ conpact
                f (a, b) = (a `intersection` b) `member` o && (a `union` b) `member` o
                ps = Set.filter (\(a, b) -> a < b) $ cartesianProduct o' o'
                ok = toList $ Set.map f ps
    
main :: IO ()    
main = mapM_ go $ zip [1..] $ openSets [0,1,2]
    where
        go (i, ln) = putStrLn $ show i ++ " : " ++ show ln
