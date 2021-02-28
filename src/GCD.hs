module GCD where

import Prelude hiding (gcd)

import Control.Monad.Trans.State (State, runState, get, put)
import Control.Monad.ST
import Data.STRef

gcd (x, y) | x == y = x
           | x < y = gcd (x, y - x)
           | x > y = gcd (x - y, y)

-- State
stateGCD (x, y) = fst $ runState stateLoop (x, y)
stateLoop :: State (Int, Int) Int
stateLoop = do { (x, y) <- get
          ; if x == y then return x
            else if x < y then do { put (x, y - x)
                                  ; stateLoop
                                  }
                 else do { put (x - y, y)
                         ; stateLoop
                         }
          }

-- ST
stGCD (x, y) = runST $ do
  a <- newSTRef x
  b <- newSTRef y
  stLoop a b

stLoop :: STRef s Int -> STRef s Int -> ST s Int
stLoop a b = do
  x <- readSTRef a
  y <- readSTRef b
  if x == y then return x
    else if x < y then do { writeSTRef b (y - x)
                          ; stLoop a b
                          }
         else do { writeSTRef a (x - y)
                 ; stLoop a b
                 }

ordering lt eq gt LT = lt
ordering lt eq gt EQ = eq
ordering lt eq gt GT = gt
