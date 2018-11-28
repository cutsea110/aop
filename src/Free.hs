{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, RankNTypes, TupleSections #-}
-- Ref.) https://stackoverflow.com/questions/13352205/what-are-free-monads/13352580
module Free where


data Free f a = Pure a
              | Roll (f (Free f a))

pure' = Pure
roll = Roll

--    In = [pure, roll]
-- Ta <-------------- a + F(Ta)
-- |                    |
-- | u                  | 1 + Fu
-- |                    |
-- v                    v
-- X  <-------------- a + FX
--       [f, g]

cata (f, g) (Pure x) = f x
cata (f, g) (Roll x) = g (fmap (cata (f, g)) x) -- this fmap is over F not T (= Free f)

instance Functor f => Functor (Free f) where
    fmap f = cata (pure' . f, roll)
--     fmap f (Pure x) = Pure (f x)
--     fmap f (Roll x) = Roll (fmap (fmap f) x)

concatFree :: Functor f => Free f (Free f a) -> Free f a
concatFree = cata (id, roll)
-- concatFree (Pure x) = x
-- concatFree (Roll x) = Roll (fmap concatFree x)

-- Ref.) https://stackoverflow.com/questions/22121960/applicative-instance-for-free-monad
instance Functor f => Applicative (Free f) where
    pure = pure'
    Pure f <*> x = fmap f x
    Roll f <*> x = Roll (fmap (<*> x) f)

instance (Functor f, Applicative (Free f)) => Monad (Free f) where
    return = pure
    x >>= f = concatFree (f <$> x)

-- example.
-- L(A) = F(GA, HA) where G,H are unary functor.
-- phi :: H <- L imples H <- TG where (alpha, T) is initial type over F(AOP exercise 2.39)
-- 
-- And then, consider the case when substituted G to Identity functor,and substitute H to `B'.
-- So, the initial F-Algebra of TA <- F(A, G(TA))) means Binary Tree(Leafy). 
data B a = B a a deriving Show

instance Functor B where
    fmap f (B x y) = B (f x) (f y)

type Tree a = Free B a
tip :: a -> Tree a
tip x = pure x
bin :: Tree a -> Tree a -> Tree a
bin l r = Roll (B l r)

instance Show a => Show (Tree a) where
    show (Pure a) = "Pure " ++ show a
    show (Roll (B x y)) = "Roll (B {" ++ show x ++ "," ++ show y ++ "})" 

-- >>> let tr = bin (tip 2) (tip 1)
-- >>> let tl = bin (tip 4) (tip 3)
-- >>> let t = bin (tip 5) (bin tl tr)
-- >>> let f1 = tip (*2)
-- >>> fmap (*2) t
-- >>> f1 <*> t
-- >>> let f2 = bin (tip (*2)) (tip (+2))
-- >>> f2 <*> tl
-- >>> f2 <*> t
monadTest = do
    x <- bin (tip 'a') (bin (tip 'b') (tip 'c'))
    y <- bin (bin (tip 1) (tip 2)) (tip 3)
    return (x,y)

monadTest2 = do
    f <- bin (tip (+2)) (bin (tip (*2)) (tip (^2)))
    x <- bin (bin (tip 1) (tip 2)) (tip 3)
    return (f x)

monadTest3 = do
    f <- tip $ bin (tip (+2)) (bin (tip (*2)) (tip (^2)))
    x <- tip $ bin (bin (tip 1) (tip 2)) (tip 3)
    f <*> x

{-
-- the case of f is Identity
-- F(A, TA) = A + TA
data Free a = Pure a
            | Roll (Free a)
            deriving (Show, Eq)

pure' = Pure
roll = Roll

--    In = [pure, roll]
-- Ta <-------------- a + Ta
-- |                    |
-- | u                  | 1 + u
-- |                    |
-- v                    v
-- X  <-------------- a + X
--       [f, g]

cata :: (a -> b, b -> b) -> Free a -> b
cata (f, g) (Pure x) = f x
cata (f, g) (Roll x) = g (cata (f, g) x)

-- T f = cata (alpha . F(f, id))
--     = cata ([pure, roll] . (f + id))
--     = cata (pure . f, roll)
freeMap :: (a -> b) -> Free a -> Free b
freeMap f = cata (pure' . f, roll)

instance Functor Free where
    fmap = freeMap

instance Applicative Free where
    pure = pure'
    (<*>) = cata (fmap, (roll.))

mu :: Free (Free a) -> Free a
mu = cata (id, roll)

instance Monad Free where
    return = pure
    x >>= f = f =<< x where (=<<) = (mu.).fmap
-}
