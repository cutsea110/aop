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
        isOpen x = all (==True) ok
            where
                x' = x Set.\\ conpact
                ps = Set.filter (\(a, b) -> a < b) $ cartesianProduct x' x'
                ps' = Set.map (\(a, b) -> (a `intersection` b, a `union` b)) ps
                ok = toList $ Set.map (\(a, b) -> a `member` x && b `member` x) ps'
    
main :: IO ()    
main = mapM_ go $ zip [1..] $ openSets [0,1,2]
    where
        go (i, ln) = putStrLn $ show i ++ " : " ++ show ln
