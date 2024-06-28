{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- ref: https://jtobin.io/time-traveling-recursion
module Futu where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable
import Debug.Trace (trace)

($?) :: (Show a, Show b) => (a -> b) -> a -> b
f $? x = let v = f x
             msg = show x ++ " => " ++ show v
         in trace msg v


oddIndices :: Show a => [a] -> [a]
oddIndices = histo (phi $?) where
  phi Nil                             = []
  phi (Cons h (_ :< Nil))             = [h]
  phi (Cons h (_ :< Cons _ (t :< _))) = h:t

evenIndices :: Show a => [a] -> [a]
evenIndices = histo (phi $?) where
  phi Nil                             = []
  phi (Cons _ (_ :< Nil))             = []
  phi (Cons _ (_ :< Cons h (t :< _))) = h:t

oddIndices' :: Show a => [a] -> [a]
oddIndices' = futu (psi $?) where
  psi xs = case project xs of
    Nil      -> Nil
    Cons x s -> Cons x $ do
      return $ case project s of
        Nil      -> s
        Cons _ t -> t

evenIndices' :: Show a => [a] -> [a]
evenIndices' = futu (psi $?) where
  psi xs = case project xs of
    Nil -> Nil
    Cons _ s -> case project s of
      Nil -> Nil
      Cons h t -> Cons h $ return t
