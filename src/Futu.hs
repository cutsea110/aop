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

oddIndices' :: [a] -> [a]
oddIndices' = futu psi where
  psi xs = case project xs of
    Nil      -> Nil
    Cons x s -> Cons x $ do
      return $ case project s of
        Nil      -> s
        Cons _ t -> t

evenIndices' :: [a] -> [a]
evenIndices' = futu psi where
  psi xs = case project xs of
    Nil -> Nil
    Cons _ s -> case project s of
      Nil -> Nil
      Cons h t -> Cons h $ return t
