{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Transaction where

import Control.Arrow (first)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

newtype TransactionT ctx m a = TxT { runT :: ctx -> m (a, ctx) }

instance MonadTrans (TransactionT ctx) where
  lift m = TxT $ \ctx ->
    do
      x <- m
      return (x, ctx)

instance Monad m => Monad (TransactionT cxt m) where
  tx >>= f = TxT $ \ctx ->
    do
      (x, ctx') <- runT tx ctx
      runT (f x) ctx'
instance Monad m => Functor (TransactionT ctx m) where
  fmap f tx = TxT $ \ctx ->
    do
      (x, ctx') <- runT tx ctx
      return (f x, ctx')

instance Monad m => Applicative (TransactionT ctx m) where
  pure x = TxT (\ctx -> return (x, ctx))
  f <*> tx = TxT $ \ctx ->
    do
      (f', ctx') <- runT f ctx
      (x, ctx'') <- runT tx ctx'
      return (f' x, ctx'')

instance MonadIO m => MonadIO (TransactionT ctx m) where
  liftIO = lift . liftIO

instance MonadFail m => MonadFail (TransactionT ctx m) where
  fail msg = TxT $ \ctx ->
    fail $ "TransactionT: failed\n" ++ msg

class Monad m => MonadTransaction ctx m | m -> ctx where
  txEmpty :: a -> m a
  txEmpty = return
  txBind :: m a -> (a -> m b) -> m b
  txBind = (>>=)
  txApply :: (a -> b) -> m a -> m b
  txApply = (<$>)
  txApply2 :: (a -> b -> c) -> m a -> m b -> m c
  txApply2 f tx1 tx2 = do
    x <- tx1
    y <- tx2
    return $ f x y
  txAp :: m (a -> b) -> m a -> m b
  txAp = (<*>)

instance Monad m => MonadTransaction ctx (TransactionT ctx m)
