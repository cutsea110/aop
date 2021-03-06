module FibAsHisto where

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
-- this like histo engine
psi'' n = case fmap psi'' (out n) of
  p@Nothing   -> Ano (0, p)
  p@(Just n') -> case sub n' of
    Nothing  -> Ano (1, p)
    Just n'' -> Ano (ex n' + ex n'', p)

-- extract
ex  (Ano (x, _)) = x
-- sub
sub (Ano (_, y)) = y
ana psi = In . fmap (ana psi) . psi
toNat = ana psi
  where
    psi 0 = Nothing
    psi n = Just (n-1)
{-    
toNat 0 = In Nothing
toNat n = In (Just (toNat (n-1)))
-}
exs (Ano (n, Nothing)) = [n]
exs (Ano (n, Just x))  = n:exs x

fib = ex . psi'' . toNat
