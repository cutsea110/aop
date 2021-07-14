-- | ref.) http://okmij.org/ftp/Haskell/numbered-monad.txt
{-# LANGUAGE TupleSections #-}
module SNMonad where

type Numbered a = (Int, a)

newtype NumberedM a = NumberedM { runNumberedM :: Int -> Numbered a }

instance Functor NumberedM where
  fmap f (NumberedM m)
    = NumberedM $ \n -> let (n', v) = m n
                        in (n', f v)

instance Applicative NumberedM where
  pure x = NumberedM (,x)
  -- ^ m :: Int -> (Int, a)
  -- ^ f :: Int -> (Int, a -> b)
  NumberedM f <*> NumberedM m
    = NumberedM $ \n -> let (n', v)   = m n
                            (n'', f') = f n'
                        in (n'', f' v)

instance Monad NumberedM where
  NumberedM m >>= f = NumberedM $ \n -> let (n', v) = m n
                                            NumberedM m' = f v
                                        in m' n'
  return x = NumberedM (,x)

incr :: NumberedM Int
incr = NumberedM $ \n -> (n+1, n)

