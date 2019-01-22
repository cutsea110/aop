module OpenSet where

import Data.Set as Set

e :: Set Int
e = fromList []
u :: Set Int
u = fromList [0,1,2]
conpact = fromList [e, u]

u' = powerSet u

candidates = Set.map (union conpact) $ powerSet (u' \\ conpact)

openSets = Set.filter pred candidates
    where
        pred :: Set (Set Int) -> Bool
        pred = undefined
