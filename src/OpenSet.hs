module OpenSet where

import Data.List as List ((\\))
import Data.Set as Set

e :: Set Int
e = fromList []
u :: Set Int
u = fromList [0,1,2,3]
conpact = fromList [e, u]


u' = powerSet u
candidates = Set.map (union conpact) $ powerSet (u' Set.\\ conpact)
sub = Prelude.map toList $ toList (u' Set.\\ conpact)

isOpen x = all (==True) ok
    where
        x' = x Set.\\ conpact
        ps = Set.filter (\(a, b) -> a < b) $ cartesianProduct x' x'
        ps' = Set.map (\(a, b) -> (a `intersection` b, a `union` b)) ps
        ok = toList $ Set.map (\(a, b) -> a `member` x && b `member` x) ps'

openSets = Set.filter isOpen candidates

openSets' = toList $ Set.map (toList.(Set.map toList)) openSets

main = mapM_ go $ zip [1..] openSets'
    where
        go (i, ln) = putStrLn $ show i ++ " : " ++ show ln
