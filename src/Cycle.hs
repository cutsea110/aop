module Cycle where

import Prelude hiding (cycle)

cycle :: [a] -> [a]
cycle [] = error "Oops!"
cycle xs = ys
  where ys = xs ++ ys
