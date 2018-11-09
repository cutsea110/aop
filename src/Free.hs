{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
-- Ref.) https://stackoverflow.com/questions/13352205/what-are-free-monads/13352580
module Free where


data Free f a = Pure a
              | Roll (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Roll x) = Roll (fmap (fmap f) x)

concatFree :: Functor f => Free f (Free f a) -> Free f a
concatFree (Pure x) = x
concatFree (Roll x) = Roll (fmap concatFree x)

-- Ref.) https://stackoverflow.com/questions/22121960/applicative-instance-for-free-monad
instance Functor f => Applicative (Free f) where
    pure = Pure
    Pure f <*> x = fmap f x
    Roll f <*> x = Roll (fmap (<*> x) f)

instance (Functor f, Applicative (Free f)) => Monad (Free f) where
    return = pure
    x >>= f = concatFree (f <$> x)

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

-- >>> let tr = Roll (B (Pure 2) (Pure 1))
-- >>> let tl = Roll (B (Pure 4) (Pure 3))
-- >>> let t = Roll (B (Pure 5) (Roll (B tl tr))
-- >>> let f1 = Pure (*2)
-- >>> fmap (*2) t
-- >>> f1 <*> t
-- >>> let f2 = Roll (B (Pure (*2)) (Pure (+2)))
-- >>> f2 <*> tl
-- >>> f2 <*> t

{--
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
    fmap f (Pure x) = Pure (f x)
    fmap f (Roll x) = Roll (fmap f x)

instance Applicative Free where
    pure = pure'
    Pure f <*> x = fmap f x
    Roll f <*> x = Roll (f <*> x)

mu :: Free (Free a) -> Free a
mu (Pure x) = x
mu (Roll x) = Roll (mu x)

instance Monad Free where
    return = pure
    x >>= f = mu (f <$> x)
--}
