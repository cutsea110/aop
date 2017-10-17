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

para (c, g) Zero = c
para (c, g) (Succ n) = g n (para (c, g) n)

plus x = foldn (x, Succ)
mult x = foldn (Zero, plus x)
expr x = foldn (Succ Zero, mult x)
fact = para (Succ Zero, mult')
  where
    mult' n m = mult (Succ n) m

toNat = unfoldn (\n -> if n <= 0 then Nothing else Just (n-1))
fromNat = foldn (0, (1+))

succ = Succ
succ' (Succ n) = n

-- coreflexive over (Succ n) but Zero
positive = succ . succ'

-- coreflexive over Zero but (Succ n)
corefZero = foldn (Zero, positive)

-- const Zero
constZero = foldn (Zero, id)

-- the converse of corefZero equals corefZero,
-- because corefZero can be represented by [(Zero, Zero)] on REL.
corefZero' = corefZero

x = constZero . corefZero'
