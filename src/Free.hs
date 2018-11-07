{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
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

instance (Functor f,Applicative (Free f)) => Monad (Free f) where
    return = pure
    x >>= f = concatFree (f <$> x)
