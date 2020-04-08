module TakePrefix where

import Data.List (inits)

takePrefix :: Ord a => ([a] -> Bool) -> [a] -> [a]
takePrefix p = maximum . filter p . inits

