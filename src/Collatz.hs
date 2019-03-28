{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Collatz where

-- We would like to do this!
psi Nothing = (0, Nothing) -- 0
-- psi (Just Nothing) = (1, Just Nothing) -- NOT this
psi (Just Nothing) = (1, Just (0, Nothing)) -- 1
psi (Just (Just Nothing)) = (1, Just (1, Just (0, Nothing))) -- 2
psi (Just (Just (Just Nothing))) = (2, Just (1, Just (1, Just (0, Nothing)))) -- 3
psi (Just (Just (Just (Just Nothing)))) = (3, Just (2, Just (1, Just (1, Just (0, Nothing))))) -- 4
psi (Just (Just (Just (Just (Just Nothing))))) = (5, Just (3, Just (2, Just (1, Just (1, Just (0, Nothing)))))) -- 5
psi (Just (Just (Just (Just (Just (Just Nothing)))))) = (8, Just (5, Just (3, Just (2, Just (1, Just (1, Just (0, Nothing))))))) -- 6

{-- couldn't typable
psi' Nothing = (0, Nothing)
psi' (Just Nothing) = (1, Just (psi' Nothing))
psi' (Just n) = case psi' n of
  p@(f1, Just (f2, mv)) -> (f1 + f2, Just p)
--}

type Nat = Fix Maybe

data Fix f = In { out :: f (Fix f) }

newtype Ano = Ano (Integer, Maybe Ano)

-- fit a type
psi' :: Nat -> Ano
psi' (In Nothing) = Ano (0, Nothing) -- 0
psi' (In (Just (In Nothing))) = Ano (1, Just (Ano (0, Nothing))) -- 1
psi' (In (Just (In (Just (In Nothing))))) = Ano (1, Just (Ano (1, Just (Ano (0, Nothing))))) -- 2

-- done!
psi'' (In Nothing) = Ano (0, Nothing)
psi'' (In (Just p@(In Nothing))) = Ano (1, Just (psi'' p))
psi'' (In (Just n)) = case psi'' n of
  p@(Ano (f1, Just (Ano (f2, mv)))) -> Ano (f1 + f2, Just p)

ex (Ano (x, _)) = x
toNat 0 = In Nothing
toNat n = In (Just (toNat (n-1)))

exs (Ano (n, Nothing)) = [n]
exs (Ano (n, Just x))  = n:exs x
