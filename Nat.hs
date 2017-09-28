import Prelude hiding (succ)

data Nat = Zero
         | Succ Nat
         deriving (Show, Eq)

foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) Zero = c
foldn (c, f) (Succ n) = f (foldn (c, f) n)

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn phi x = case phi x of
  Nothing -> Zero
  Just x' -> Succ (unfoldn phi x')

toNat = unfoldn (\n -> if n <= 0 then Nothing else Just (n-1))

succ = Succ
succ' (Succ n) = n

-- coreflexive over (Succ n) but Zero
positive = succ . succ'

-- coreflexive over Zero but (Succ n)
corefZero = foldn (Zero, positive)

-- const Zero
constZero = foldn (Zero, id)

-- what?
-- the converse of corefZero
corefZero' = undefined
