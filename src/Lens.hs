{-# LANGUAGE RankNTypes #-}
module Lens where

-- コモナド (余モナド)
class Functor w => Comonad w where
  -- dual to return on Monad
  extract :: w a -> a
  -- dual to join on Monad
  duplicate :: w a -> w (w a)

-- 余状態コモナド
data Store b a = Store (b -> a) b

instance Functor (Store b) where
  fmap f (Store v b) = Store (f . v) b

instance Comonad (Store b) where
  extract (Store v b) = v b
  duplicate (Store v b) = Store (Store v) b

type Lens s t a b = forall r. (a -> Store r b) -> (s -> Store r t)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = let Store v r = f (get s)
                   in Store (\_ -> set s (v r)) r

get :: Lens s s a a -> s -> a
get f s = let Store _ a = f (\a -> Store undefined a) s
          in a
set :: Lens s t a b -> s -> b -> t
set f s b = let Store v r = f (Store (const b)) s
            in v r

_fst :: Lens (a, x) (b, x) a b
_fst = lens fst setFst
  where setFst (_, x) y = (y, x)
_snd :: Lens (x, a) (x, b) a b
_snd = lens snd setSnd
  where setSnd (x, _) y = (x, y)

-- >>> get _fst (1,2)
-- 1

-- >>> get (_fst._snd) ((1,2),3)
-- 2

-- >> set (_fst._snd) ((1,2),3) 100
-- ((1,100),3)
