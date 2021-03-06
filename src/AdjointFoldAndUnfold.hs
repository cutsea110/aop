{-# LANGUAGE RankNTypes #-}
module AdjointFoldAndUnfold where
-- | ref.) http://www.cs.ox.ac.uk/ralf.hinze/SSGIP10/AdjointFolds.pdf
--         https://www.researchgate.net/publication/221440236_Adjoint_Folds_and_Unfolds
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

{--
-- Example 3.1
--
-- ListF X = 1 + Id * X
--
data Base a b = Nil | Cons (a, b)
type ListF x a = Base a (x a)
newtype List a = In { out :: ListF List a }

--
-- data List a = Nil | Cons (a, List a)
--

base :: (a -> c) -> (b -> d) -> Base a b -> Base c d
base f g Nil = Nil
base f g (Cons (x, y)) = Cons (f x, g y)

hfold :: (forall a. ListF n a -> n a) -> List b -> n b
hfold f = f . listF (hfold f) . out

listF :: (forall a. x a -> y a) -> ListF x b -> ListF y b
listF f = base id f
--}

{--
-- Example 3.2
--
-- NestF X = 1 + Id * (X . Pair)
--
data Base a b = Nil | Cons (a, b)
type NestF x a = Base a (x (Pair a))
type Pair a = (a, a)
newtype Nest a = In { out :: NestF Nest a }

--
-- data Nest a = Nil | Cons (a, Nest (a, a))
--

base :: (a -> c) -> (b -> d) -> Base a b -> Base c d
base f g Nil = Nil
base f g (Cons (x, y)) = Cons (f x, g y)

hfold :: (forall a. Base a (n (Pair a)) -> n a) -> Nest b -> n b
hfold f = f . base id (hfold f) . out

nest :: (a -> b) -> Nest a -> Nest b
nest f = In . base f (nest (pair f)) . out

pair :: (a -> b) -> Pair a -> Pair b
pair f (x, y) = (f x, f y)
--}

{--
-- Example 3.3
--
-- HostF X = 1 + Id * (X . (Id * X))
--
data Base a b = Nil | Cons (a, b)
type HostF x a = Base a (x (a, x a))
newtype Host a = In { out :: HostF Host a }

--
-- data Host a = Nil | Cons (a, Host (a, Host a))
--

base :: (a -> c) -> (b -> d) -> Base a b -> Base c d
base f g Nil = Nil
base f g (Cons (x, y)) = Cons (f x, g y)

hfold :: (forall a. Base a (n (a, n a)) -> n a) -> Host b -> n b
hfold f = f . base id (hfold f . host (id *** hfold f)) . out

host :: (a -> b) -> Host a -> Host b
host f = In . base f (host (f *** host f)) . out

(***) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(f *** g) (a, b) = (f a, g b)
--}

{--
-- Example 4.1
-- Generalized
-- F X = B . <Id, X . F_1 X, X . F_2 X, ...>

--
-- ListF X = 1 + Id * X
--
-- Base X Y = 1 + X * Y
-- ListF X = Base . <Id, X . F_1 X> == ListF x a = Base a (x (f1 x a)) ==> Base a (x a)
-- F_1 X = Id                       == f1 x a = a
--
--                   out
-- List A  ------------------------> 1 + A x List A
--    |                                |
--    | (|f|)                          | 1 + 1_A x (|f|)
--    v                                v
--   n A   ------------------------> 1 + A x n A
--                     f
--
data Base a b = Nil | Cons (a, b)
type ListF x a = Base a (x a)
newtype List a = In { out :: ListF List a }

--
-- data List a = Nil | Cons (a, List a)
--

base :: (a -> c) -> (b -> d) -> Base a b -> Base c d
base f g Nil = Nil
base f g (Cons (x, y)) = Cons (f x, g y)

hfold :: (forall a. ListF n a -> n a) -> List b -> n b
hfold f = f . listF (hfold f) . out

listF :: (forall a. x a -> y a) -> ListF x b -> ListF y b
listF f = base id f

list :: (a -> b) -> List a -> List b
list f = In . base f (list f) . out

gfold :: (forall a. Base (m a) (n a) -> n a)
      -> (forall a. m a -> m a)
      -> List (m b) -> n b
gfold f g = f . base id (gfold f g . list g) . out
--}

{--
-- Example 4.2
-- Generalized
-- F X = B . <Id, X . F_1 X, X . F_2 X, ...>
--
-- NestF X = 1 + Id * (X . Pair)
--
-- Base X Y = 1 + X * Y
-- NestF X = Base . <Id, X . F_1 X> == HostF x a = Base a (x (f1 x a)) ==> Base a (x (Pair a))
-- F_1 X = Pair                     == f1 x a = Pair a
--

--
-- o hfold
--                    out
--  Nest A  ------------------------> 1 + A x Nest (Pair A)
--     |                                   |
--     | (|f|)                             | 1 + 1_A x (|f|)
--     v                                   v
--    n A   <------------------------ 1 + A x  n (Pair A)
--                      ^
--                      |
--                      v
--    n a   <------------------------ 1 + a x  n a   forall a.
--                      f
--   * f :: forall a. 1 + a x n (Pair a) -> n a
--
-- o nest
--                                out
--    A   Nest A  --------------------------------> 1 + A x Nest (Pair A)
--    |                                                   |
--    |    nest f                                         | 1 + 1_A x nest (pair f)
--    v                                                   v
--    B   Nest B <------ 1 + B x n (Pair B) <------ 1 + A x Nest (Pair B)
--                  In                      1 + f x id

data Base a b = Nil | Cons (a, b)
type NestF x a = Base a (x (Pair a))
type Pair a = (a, a)
newtype Nest a = In { out :: NestF Nest a }

--
-- data Nest a = Nil | Cons (a, Nest (a, a))
--

base :: (a -> c) -> (b -> d) -> Base a b -> Base c d
base f g Nil = Nil
base f g (Cons (x, y)) = Cons (f x, g y)

hfold :: (forall a. Base a (n (Pair a)) -> n a) -> Nest b -> n b
hfold f = f . base id (hfold f) . out

nest :: (a -> b) -> Nest a -> Nest b
nest f = In . base f (nest (pair f)) . out

pair :: (a -> b) -> Pair a -> Pair b
pair f (x, y) = (f x, f y)

gfold :: (forall a. Base (m a) (n (Pair a)) -> n a)
      -> (forall a. Pair (m a) -> m (Pair a))
      -> Nest (m b) -> n b
gfold f g = f . base id (gfold f g . nest g) . out
--}

{--
-- Example 4.3
--
-- HostF X = 1 + Id * (X . (Id * X))
--
-- o hfold
--                           out
--  Host A  -------------------------------------> 1 + A x Host (A x Host A)
--     |                                                |
--     |                                                |  1 + 1_A x host (id_A x (|f|))
--     |                                                v
--     | (|f|)                                     1 + A x Host (A x   n A)
--     |                                                |
--     |                                                |  1 + 1_A x (|f|)
--     v                                                v
--    n A   <------------------------------------- 1 + A x n (A x n A)
--                             f
--
--                            out
--   Host A   ----------------------------------> 1 + A x Host A
--     |                                                |
--     | host f                                         |  1 + 1_A x host f
--     v                                                v
--   Host B   <-------- 1 + B x Host B <--------- 1 + A x Host B
--               In                   1 + f x id
--
data Base a b = Nil | Cons (a, b)
type HostF x a = Base a (x (a, x a))
newtype Host a = In { out :: HostF Host a }

--
-- data Host a = Nil | Cons (a, Host (a, Host a))
--
-- Base X Y = 1 + X * Y
-- HostF X = Base . <Id, X . F_1 X> == HostF x a = Baes a (x (f1 x a))  ==> Base a (x (a, x a))
-- F_1 X = * . <Id, X . F_2 X>      == f1 x a = (a, x (f2 x a))         ==> (a, x a)
-- F_2 X = Id                       == f2 x a = a
--

base :: (a -> c) -> (b -> d) -> Base a b -> Base c d
base f g Nil = Nil
base f g (Cons (x, y)) = Cons (f x, g y)

hfold :: (forall a. Base a (n (a, n a)) -> n a) -> Host b -> n b
hfold f = f . base id (hfold f . host (id *** hfold f)) . out

host :: (a -> b) -> Host a -> Host b
host f = In . base f (host (f *** host f)) . out

(***) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(f *** g) (a, b) = (f a, g b)

gfold :: (forall a. Base (m a) (n (a, n a)) -> n a)
      -> (forall a. (m a, n a) -> m (a, n a))
      -> (forall a. m a -> m a)
      -> Host (m b) -> n b
gfold f g1 g2 = f . base id (gfold f g1 g2 . host (g1 . (id *** (gfold f g1 g2 . host g2)))) . out
--}
