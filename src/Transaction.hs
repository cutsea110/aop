{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Transaction where

import Control.Arrow (first)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

newtype TransactionT ctx m a = TxT { runT :: ctx -> m (a, ctx) }
instance MonadTrans (TransactionT ctx) where
  lift m = TxT (\ctx -> do
                   x <- m
                   return (x, ctx))
instance Monad m => Monad (TransactionT cxt m) where
  tx >>= f = TxT (\ctx -> do
                      (x, ctx') <- runT tx ctx
                      runT (f x) ctx')
instance Monad m => Functor (TransactionT ctx m) where
  fmap f tx = TxT (\ctx -> do
                      (x, ctx') <- runT tx ctx
                      return (f x, ctx'))

instance Monad m => Applicative (TransactionT ctx m) where
  pure x = TxT (\ctx -> return (x, ctx))
  f <*> tx = TxT (\ctx -> do
                       (f', ctx') <- runT f ctx
                       (x, ctx'') <- runT tx ctx'
                       return (f' x, ctx''))

instance MonadIO m => MonadIO (TransactionT ctx m) where
  liftIO = lift . liftIO

instance MonadFail m => MonadFail (TransactionT ctx m) where
  fail msg = TxT (\ctx -> fail $ "TransactionT: failed\n" ++ msg)

class Monad m => MonadTransaction ctx m | m -> ctx where
  begin :: ctx -> m a
  commit :: m a -> m ()
  rollback :: m a -> m ()

type Transaction ctx a = TransactionT ctx IO a

{-
newtype Transaction ctx e a = Tx { run :: ctx -> Either e (a, ctx) }

-- return
txEmpty :: a -> Transaction ctx e a
txEmpty x = Tx (\ctx -> Right (x, ctx))

-- (=<<)
andThen :: Transaction ctx e a -> (a -> Transaction ctx e b) -> Transaction ctx e b
tx `andThen` f = Tx g
  where g ctx = run <$> f . fst <*> snd =<< run tx ctx

txApply :: (a -> b) -> Transaction ctx e a -> Transaction ctx e b
f `txApply` tx = Tx g
  where g ctx = first f <$> run tx ctx

txApply2 :: (a -> b -> c) -> Transaction ctx e a -> Transaction ctx e b -> Transaction ctx e c
txApply2 f tx1 tx2 = Tx g
  where g ctx = do
          (x, ctx')  <- run tx1 ctx
          (y, ctx'') <- run tx2 ctx'
          return (f x y, ctx'')

txAp :: Transaction ctx e (a -> b) -> Transaction ctx e a -> Transaction ctx e b
tf `txAp` tx = txApply2 ($) tf tx

txAlt :: Transaction ctx e a -> Transaction ctx e a -> Transaction ctx e a
tx1 `txAlt` tx2 = Tx g
  where g ctx = case run tx1 ctx of
          Left _ -> run tx2 ctx
          r      -> r
-}
