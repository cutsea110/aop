module Transaction where

import Control.Arrow (first)

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
