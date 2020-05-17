module AdjointFoldAndUnfold where

import Numeric.Natural

data Stack s = Empty
             | Push (Natural, s)
             deriving Show
instance Functor Stack where
  fmap f Empty = Empty
  fmap f (Push (n, s)) = Push (n, f s)
