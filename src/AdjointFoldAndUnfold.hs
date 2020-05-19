{-# LANGUAGE RankNTypes #-}
module AdjointFoldAndUnfold where
{--
import Numeric.Natural

data Stack s = Empty
             | Push (Natural, s)
             deriving Show
instance Functor Stack where
  fmap f Empty = Empty
  fmap f (Push (n, s)) = Push (n, f s)

newtype Fix f = In { unIn :: f (Fix f) }
newtype Cofix f = UnOut { out :: f (Cofix f) }

total :: Fix Stack -> Natural
total (In Empty) = 0
total (In (Push (n, s))) = n + total s

-- | Mendler-style
-- psi == total'
-- x   == ttl
-- a   == Empty and Pust (n, s)
total' :: (t -> Natural) -> Stack t -> Natural
total' ttl Empty = 0
total' ttl (Push (n, s)) = n + ttl s

ttl :: Fix Stack -> Natural
ttl (In s) = total' ttl s    -- Mendler-style equation : x (In a) == psi x a

data Sequ s = Next (Natural, s) deriving Show
instance Functor Sequ where
  fmap f (Next (n, s)) = Next (n, f s)

from :: Natural -> Cofix Sequ
from n = UnOut (Next (n, from (n+1)))

-- | Mendler-style
-- psi == from'
-- x   == frm
-- a   == n
from' :: (Natural -> s) -> Natural -> Sequ s
from' frm n = Next (n, frm (n+1))

frm :: Natural -> Cofix Sequ
frm n = UnOut (from' frm n)  -- Mendler-style equation : x a = UnOut (psi x a)

data Pow a = Zero a
           | Succ (Pow (Pair a))
           deriving Show
type Pair a = (a, a)


data Base a b = Nil
              | Cons (a, b)
              deriving Show
type ListF x a = Base a (x a)
newtype List a = InList (ListF List a)
--}

data Base a b = Nil | Cons (a, b)
type NestF x a = Base a (x (Pair a))
type Pair a = (a, a)
newtype Nest a = In { out :: NestF Nest a }

base :: (a -> c) -> (b -> d) -> Base a b -> Base c d
base f g Nil = Nil
base f g (Cons (x, y)) = Cons (f x, g y)

hfold :: (forall a. Base a (n (Pair a)) -> n a) -> Nest b -> n b
hfold f = f . base id (hfold f) . out

nest :: (a -> b) -> Nest a -> Nest b
nest f = In . base f (nest (pair f)) . out

pair :: (a -> b) -> Pair a -> Pair b
pair f (x, y) = (f x, f y)
