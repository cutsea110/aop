module AdjointFoldAndUnfold where

import Numeric.Natural

data Stack s = Empty
             | Push (Natural, s)
             deriving Show
instance Functor Stack where
  fmap f Empty = Empty
  fmap f (Push (n, s)) = Push (n, f s)

newtype Fix f = In { unIn :: f (Fix f) }
newtype Cofix f = Out { unout :: f (Cofix f) }

total :: Fix Stack -> Natural
total (In Empty) = 0
total (In (Push (n, s))) = n + total s
