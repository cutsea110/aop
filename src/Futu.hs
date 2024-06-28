{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- ref: https://jtobin.io/time-traveling-recursion
module Futu where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable

oddIndices :: [a] -> [a]
oddIndices = histo $ \case
  Nil                           -> []
  Cons h (_ :< Nil)             -> [h]
  Cons h (_ :< Cons _ (t :< _)) -> h:t

evenIndices :: [a] -> [a]
evenIndices = histo $ \case
  Nil                           -> []
  Cons _ (_ :< Nil)             -> []
  Cons _ (_ :< Cons h (t :< _)) -> h:t
