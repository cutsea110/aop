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
  phi :: Show a => ListF a (Cofree (ListF a) [a]) -> [a]
  phi Nil                             = []
  phi (Cons h (_ :< Nil))             = [h]
  phi (Cons h (_ :< Cons _ (t :< _))) = h:t

evenIndices :: Show a => [a] -> [a]
evenIndices = histo (phi $?) where
  phi :: Show a => ListF a (Cofree (ListF a) [a]) -> [a]
  phi Nil                             = []
  phi (Cons _ (_ :< Nil))             = []
  phi (Cons _ (_ :< Cons h (t :< _))) = h:t

oddIndices' :: Show a => [a] -> [a]
oddIndices' = futu (psi $?) where
  psi :: Show a => [a] -> ListF a (Free (ListF a) [a])
  psi xs = case project xs of
    Nil      -> Nil
    Cons x s -> Cons x $ do
      return $ case project s of
        Nil      -> s
        Cons _ t -> t

evenIndices' :: Show a => [a] -> [a]
evenIndices' = futu (psi $?) where
  psi :: Show a => [a] -> ListF a (Free (ListF a) [a])
  psi xs = case project xs of
    Nil -> Nil
    Cons _ s -> case project s of
      Nil -> Nil
      Cons h t -> Cons h $ return t

accum :: (Show a, Num a) => [a] -> [a]
accum = futu psi where
  psi :: (Show a, Num a) => [a] -> ListF a (Free (ListF a) [a])
  psi xs = case project xs of
    Nil -> Nil
    Cons x s -> Cons (x + sum s) $ return $? s

data Frame = Strike | Spare Int | Open [Int]
  deriving (Show)

frames :: [Int] -> [Frame]
frames = futu psi where
  psi :: [Int] -> ListF Frame (Free (ListF Frame) [Int])
  psi xs = case project xs of
    Nil -> Nil
    Cons 10 s -> Cons Strike $ return s
    Cons  x s -> case project s of
      Nil -> Nil
      Cons y t -> if x + y == 10
                  then Cons (Spare x) $ return t
                  else Cons (Open [x,y]) $ return t

test :: [Int]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]
