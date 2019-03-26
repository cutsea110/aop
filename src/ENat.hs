-- Embeded Nat
module ENat where

type Nat = Int

foldn (c, f) = u
  where
    u 0 = c
    u n = f (u (n-1))

-- succ n = n + 1
plus n = foldn (n, succ)
mult n = foldn (0, (plus n))
expr n = foldn (1, (mult n))

unfoldn psi = u
  where
    u x = case psi x of
      Nothing -> 0
      Just x' -> succ (u x')
