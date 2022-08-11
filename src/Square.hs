{-# LANGUAGE RankNTypes, LambdaCase, DeriveFunctor #-}
module Square where
-- https://twitter.com/xgrommx/status/1557119985750786048

import Control.Monad.Fix
import Data.Fix hiding (cata)

data Square f a b = Square { higher :: f a -> f b
                           --           |      |
                           --           v      v
                           , lower  ::  a  ->  b
                           }

square :: forall f a b. Functor f => (a -> b) -> Square f a b
square f = Square { higher = fmap f, lower = f }

squareFix :: Functor f => (f b -> b) -> Square f (Fix f) b
squareFix alg = fix $ \g -> square (alg . higher g . unFix)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = lower (squareFix alg)

data SListF a b = SNilF | SConsF a b deriving Functor

type List a = Fix (SListF a)

nil :: List a
nil = Fix SNilF

cons :: a -> List a -> List a
cons a b = Fix (SConsF a b)

-- >>> lengthL (cons 10 $ cons 20 $ cons 30 $ nil)
-- 3
lengthL :: List a -> Int
lengthL = cata $ \case
  SNilF -> 0
  SConsF _ b -> 1 + b
