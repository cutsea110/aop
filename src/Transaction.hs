module Transaction where

newtype Transaction ctx e a = Tx { run :: ctx -> Either e (a, ctx) }

txEmpty :: a -> Transaction ctx e a
txEmpty x = Tx (\ctx -> Right (x, ctx))

andThen :: Transaction ctx e a -> (a -> Transaction ctx e b) -> Transaction ctx e b
tx `andThen` f = Tx g
  where g ctx = case run tx ctx of
                  Left e -> Left e
                  Right (x, ctx') -> run (f x) ctx'
