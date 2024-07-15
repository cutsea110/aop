{-# LANGUAGE TypeFamilies #-}
-- ref: https://jtobin.io/time-traveling-recursion
module Futu where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable
import Debug.Trace (trace)

($?) :: (Show a, Show b) => (a -> b) -> a -> b
f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ " -}"
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

average :: (Show a, Fractional a) => [a] -> a
average = uncurry (/) . foldr phi (0, 0) where phi x (s, l) = (s + x, l + 1)

movingAverage :: (Show a, Fractional a) => Int -> [a] -> [a]
movingAverage n = futu psi where
  psi :: (Show a, Fractional a) => [a] -> ListF a (Free (ListF a) [a])
  psi xs = case project xs of
    Nil -> Nil
    Cons x s -> Cons (average $ take n (x:s)) $ return s

data Frame = Strike [Int] | Spare [Int] [Int] | Open [Int] deriving (Show)

frames :: [Int] -> [Frame]
frames = futu psi where
  psi :: [Int] -> ListF Frame (Free (ListF Frame) [Int])
  psi xs = case project xs of
    Nil -> Nil
    Cons 10 s -> case project s of
      Nil -> Cons (Strike []) $ return s
      Cons x t -> case project t of
        Nil -> Cons (Strike [x]) $ return s
        Cons y u -> Cons (Strike [x,y]) $ return s
    Cons  x s -> case project s of
      Nil -> Cons (Open [x]) $ return s
      Cons y t | x + y == 10 -> case project t of
                  Nil -> Cons (Spare [x] []) $ return t
                  Cons z u -> Cons (Spare [x,y] [z]) $ return t
               | otherwise   -> Cons (Open  [x,y]) $ return t

test :: [Int]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]



nil :: Free (ListF a) b
nil = liftF Nil

cons :: a -> b -> Free (ListF a) b
cons h t = liftF (Cons h t)

twiddle :: Show a => [a] -> [a]
twiddle = futu (psi $?) where
  psi :: Show a => [a] -> ListF a (Free (ListF a) [a])
  psi r = case project r of
    Nil      -> Nil
    Cons x l -> case project l of
      Nil      -> Cons x $? nil
      Cons h t -> Cons h $? cons x t
