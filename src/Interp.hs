{-# LANGUAGE Haskell2010 #-}
-- | ref.) https://jgbm.github.io/eecs762f19/papers/wadler-monads.pdf
module Interp where

import Data.List (intercalate)

type Name = String

{- | Variation 0: Standard Interpreter -}
data TermI = VarI Name
           | ConI Int
           | AddI TermI TermI
           | LamI Name TermI
           | AppI TermI TermI

data ValueI = WrongI
            | NumI Int
            | FunI (ValueI -> I ValueI)

type EnvironmentI = [(Name, ValueI)]

showvalI :: ValueI -> String
showvalI WrongI = "<wrong>"
showvalI (NumI i) = show i
showvalI (FunI _) = "<function>"

interpI :: TermI -> EnvironmentI -> I ValueI
interpI (VarI x) e = lookupI x e
interpI (ConI i) _ = unitI (NumI i)
interpI (AddI u v) e = interpI u e `bindI` (\a ->
                       interpI v e `bindI` (\b ->
                       addI a b))
interpI (LamI x v) e = unitI (FunI (\a -> interpI v ((x, a) : e)))
interpI (AppI t u) e = interpI t e `bindI` (\f ->
                       interpI u e `bindI` (\a ->
                       applyI f a))

lookupI :: Name -> EnvironmentI -> I ValueI
lookupI x [] = unitI WrongI
lookupI x ((y, b) : e)
  | x == y    = unitI b
  | otherwise = lookupI x e

addI :: ValueI -> ValueI -> I ValueI
addI (NumI i) (NumI j) = unitI (NumI (i + j))
addI _        _        = unitI WrongI

applyI :: ValueI -> ValueI -> I ValueI
applyI (FunI k) a = k a
applyI _        _ = unitI WrongI

testI :: TermI -> String
testI t = showI (interpI t [])

type I a = a
unitI a = a
a `bindI` k = k a
showI a = showvalI a

term0I :: TermI
term0I = AppI (LamI "x" (AddI (VarI "x") (VarI "x"))) (AddI (ConI 10) (ConI 11))


{- | Variation 1: Error Message -}
data TermE = VarE Name
           | ConE Int
           | AddE TermE TermE
           | LamE Name TermE
           | AppE TermE TermE

data ValueE = WrongE
            | NumE Int
            | FunE (ValueE -> E ValueE)

type EnvironmentE = [(Name, ValueE)]

showvalE :: ValueE -> String
showvalE WrongE = "<wrong>"
showvalE (NumE i) = show i
showvalE (FunE _) = "<function>"

interpE :: TermE -> EnvironmentE -> E ValueE
interpE (VarE x) e = lookupE x e
interpE (ConE i) _ = unitE (NumE i)
interpE (AddE u v) e = interpE u e `bindE` (\a ->
                       interpE v e `bindE` (\b ->
                       addE a b))
interpE (LamE x v) e = unitE (FunE (\a -> interpE v ((x, a) : e)))
interpE (AppE t u) e = interpE t e `bindE` (\f ->
                       interpE u e `bindE` (\a ->
                       applyE f a))

lookupE :: Name -> EnvironmentE -> E ValueE
lookupE x [] = errorE ("unbound variable: " ++ x)
lookupE x ((y, b) : e)
  | x == y    = unitE b
  | otherwise = lookupE x e

addE :: ValueE -> ValueE -> E ValueE
addE (NumE i) (NumE j) = unitE (NumE (i + j))
addE a        b        = errorE $ "should be numbers: " ++ showvalE a ++ ", " ++ showvalE b

applyE :: ValueE -> ValueE -> E ValueE
applyE (FunE k) a = k a
applyE f        _ = errorE $ "should be function: " ++ showvalE f

testE :: TermE -> String
testE t = showE (interpE t [])

data E a = Success a | Error String

unitE :: a -> E a
unitE a = Success a

errorE :: String -> E a
errorE s = Error s

bindE :: E a -> (a -> E b) -> E b
(Success a) `bindE` k = k a
(Error s)   `bindE` k = Error s

showE :: E ValueE -> String
showE (Success a) = "Success: " ++ showvalE a
showE (Error s)   = "Error: " ++ s

term0E :: TermE
term0E = AppE (LamE "x" (AddE (VarE "x") (VarE "x"))) (AddE (ConE 10) (ConE 11))
term1E :: TermE -- Error: should be function: 1
term1E = AppE (ConE 1) (ConE 2)


{- | Variation 2: Error Message with Position -}
data TermP = VarP Name
           | ConP Int
           | AddP TermP TermP
           | LamP Name TermP
           | AppP TermP TermP
           | AtP  Position TermP

data ValueP = WrongP
            | NumP Int
            | FunP (ValueP -> P ValueP)

type EnvironmentP = [(Name, ValueP)]

type Position = Int

showpos :: Position -> String
showpos p = show p

pos0 :: Position
pos0 = 0

interpP :: TermP -> EnvironmentP -> P ValueP
interpP (VarP x) e = lookupP x e
interpP (ConP i) _ = unitP (NumP i)
interpP (AddP u v) e = interpP u e `bindP` (\a ->
                       interpP v e `bindP` (\b ->
                       addP a b))
interpP (LamP x v) e = unitP (FunP (\a -> interpP v ((x, a):e)))
interpP (AppP t u) e = interpP t e `bindP` (\f ->
                       interpP u e `bindP` (\a ->
                       applyP f a))
interpP (AtP p t) e = resetP p (interpP t e)

lookupP :: Name -> EnvironmentP -> P ValueP
lookupP x [] = errorP ("unbound variable: " ++ x)
lookupP x ((y, b) : e)
  | x == y    = unitP b
  | otherwise = lookupP x e

addP :: ValueP -> ValueP -> P ValueP
addP (NumP i) (NumP j) = unitP (NumP (i + j))
addP a        b        = errorP $ "should be numbers: " ++ showvalP a ++ ", " ++ showvalP b

applyP :: ValueP -> ValueP -> P ValueP
applyP (FunP k) a = k a
applyP f        _ = errorP $ "should be function: " ++ showvalP f

resetP :: Position -> P a -> P a
resetP q m = \p -> m q

showvalP :: ValueP -> String
showvalP WrongP = "<wrong>"
showvalP (NumP i) = show i
showvalP (FunP _) = "<function>"

testP :: TermP -> String
testP t = showP (interpP t [] pos0)

showP :: E ValueP -> String
showP (Success a) = "Success: " ++ showvalP a
showP (Error s)   = "Error: " ++ s

type P a = Position -> E a

unitP :: a -> P a
unitP a = \p -> unitE a

errorP :: String -> P a
errorP s = \p -> errorE $ showpos p ++ ": " ++ s

bindP :: P a -> (a -> P b) -> P b
m `bindP` k = \p -> m p `bindE` (\x -> k x p)


term0P :: TermP
term0P = AppP (LamP "x" (AddP (VarP "x") (VarP "x"))) (AddP (ConP 10) (ConP 11))
term1P :: TermP -- Error: should be function: 1
term1P = AtP 0 (AppP (AtP 1 (LamP "x" (AtP 3 (AddP (AtP 4 (VarP "x")) (AtP 5 (VarP "x"))))))
                 (AtP 2 (AppP (AtP 6 (ConP 10)) (AtP 7 (ConP 11)))))

{- | Variation 3: State -}
data TermS = VarS Name
           | ConS Int
           | AddS TermS TermS
           | LamS Name TermS
           | AppS TermS TermS
           | CountS

data ValueS = WrongS
            | NumS Int
            | FunS (ValueS -> S ValueS)

type EnvironmentS = [(Name, ValueS)]

lookupS :: Name -> EnvironmentS -> S ValueS
lookupS x [] = unitS WrongS
lookupS x ((y, b) : e)
  | x == y    = unitS b
  | otherwise = lookupS x e

interpS :: TermS -> EnvironmentS -> S ValueS
interpS (VarS x) e = lookupS x e
interpS (ConS i) _ = unitS (NumS i)
interpS (AddS u v) e = interpS u e `bindS` (\a ->
                       interpS v e `bindS` (\b ->
                       addS a b))
interpS (LamS x v) e = unitS (FunS (\a -> interpS v ((x, a):e)))
interpS (AppS t u) e = interpS t e `bindS` (\f ->
                       interpS u e `bindS` (\a ->
                       applyS f a))
interpS CountS e = fetchS `bindS` (\i -> unitS (NumS i))

addS :: ValueS -> ValueS -> S ValueS
addS (NumS i) (NumS j) = tickS `bindS` (\() -> unitS (NumS (i+j)))
addS a        b        = unitS WrongS

applyS :: ValueS -> ValueS -> S ValueS
applyS (FunS k) a = tickS `bindS` (\() -> k a)
applyS f        _ = unitS WrongS

type S a = State -> (a, State)
type State = Int

unitS :: a -> S a
unitS a = \s0 -> (a, s0)

bindS :: S a -> (a -> S b) -> S b
m `bindS` k = \s0 -> let (a, s1) = m s0
                         (b, s2) = k a s1
                     in (b, s2)

tickS :: S ()
tickS = \s -> ((), s+1)

fetchS :: S State
fetchS = \s -> (s, s)

showS :: S ValueS -> String
showS m = let (a, s1) = m 0
          in "Value: " ++ showvalS a ++ "; " ++ "Count: " ++ showint s1

showvalS :: ValueS -> String
showvalS WrongS = "<wrong>"
showvalS (NumS i) = show i
showvalS (FunS _) = "<function>"

showint :: Int -> String
showint i = show i

testS :: TermS -> String
testS t = showS (interpS t [])

term0S :: TermS
term0S = AppS (LamS "x" (AddS (VarS "x") (VarS "x"))) (AddS (ConS 10) (ConS 11))
term1S :: TermS
term1S = AddS (AddS (ConS 1) (ConS 2)) CountS


{- | Variation 4: Output -}
data TermO = VarO Name
           | ConO Int
           | AddO TermO TermO
           | LamO Name TermO
           | AppO TermO TermO
           | OutO TermO

data ValueO = WrongO
            | NumO Int
            | FunO (ValueO -> O ValueO)

type EnvironmentO = [(Name, ValueO)]


lookupO :: Name -> EnvironmentO -> O ValueO
lookupO x [] = unitO WrongO
lookupO x ((y, b) : e)
  | x == y    = unitO b
  | otherwise = lookupO x e
  

interpO :: TermO -> EnvironmentO -> O ValueO
interpO (VarO x) e = lookupO x e
interpO (ConO i) _ = unitO (NumO i)
interpO (AddO u v) e = interpO u e `bindO` (\a ->
                       interpO v e `bindO` (\b ->
                       addO a b))
interpO (LamO x v) e = unitO (FunO (\a -> interpO v ((x, a):e)))
interpO (AppO t u) e = interpO t e `bindO` (\f ->
                       interpO u e `bindO` (\a ->
                       applyO f a))
interpO (OutO u) e = interpO u e `bindO` (\a ->
                     outO a      `bindO` (\() ->
                     unitO a))
addO :: ValueO -> ValueO -> O ValueO
addO (NumO i) (NumO j) = unitO (NumO (i+j))
addO a        b        = unitO WrongO

applyO :: ValueO -> ValueO -> O ValueO
applyO (FunO k) a = k a
applyO f        _ = unitO WrongO


type Output = String
type O a = (Output, a)

unitO :: a -> O a
unitO a = ("", a)

bindO :: O a -> (a -> O b) -> O b
m `bindO` k = let (r, a) = m
                  (s, b) = k a
              in (r++s, b)

outO :: ValueO -> O ()
outO a = (showvalO a ++ "; ", ())
                 
showO (s, a) = "Output: " ++ s ++ "Value: " ++ showvalO a

showvalO :: ValueO -> String
showvalO WrongO = "<wrong>"
showvalO (NumO i) = show i
showvalO (FunO _) = "<function>"

testO :: TermO -> String
testO t = showO (interpO t [])

term0O :: TermO
term0O = AppO (LamO "x" (AddO (VarO "x") (VarO "x"))) (AddO (ConO 10) (ConO 11))
term1O :: TermO
term1O = AddO (OutO (ConO 41)) (OutO (ConO 1))

{- | Variation 5: Non-deterministic choice -}
data TermL = VarL Name
           | ConL Int
           | AddL TermL TermL
           | LamL Name TermL
           | AppL TermL TermL
           | AmbL TermL TermL
           | FailL

data ValueL = WrongL
            | NumL Int
            | FunL (ValueL -> L ValueL)

type EnvironmentL = [(Name, ValueL)]

lookupL :: Name -> EnvironmentL -> L ValueL
lookupL x [] = unitL WrongL
lookupL x ((y, b) : e)
  | x == y    = unitL b
  | otherwise = lookupL x e

interpL :: TermL -> EnvironmentL -> L ValueL
interpL (VarL x) e = lookupL x e
interpL (ConL i) _ = unitL (NumL i)
interpL (AddL u v) e = interpL u e `bindL` (\a ->
                       interpL v e `bindL` (\b ->
                       addL a b))
interpL (LamL x v) e = unitL (FunL (\a -> interpL v ((x, a):e)))
interpL (AppL t u) e = interpL t e `bindL` (\f ->
                       interpL u e `bindL` (\a ->
                       applyL f a))
interpL (AmbL u v) e = interpL u e `plusL` interpL v e
interpL FailL _ = zeroL

addL :: ValueL -> ValueL -> L ValueL
addL (NumL i) (NumL j) = unitL (NumL (i+j))
addL a        b        = unitL WrongL

applyL :: ValueL -> ValueL -> L ValueL
applyL (FunL k) a = k a
applyL f        _ = unitL WrongL


type L a = [a]

unitL :: a -> L a
unitL a = [a]

bindL :: L a -> (a -> L b) -> L b
m `bindL` k = [b | a <- m, b <- k a]

zeroL :: L a
zeroL = []
plusL :: L a -> L a -> L a
l `plusL` m = l ++ m
showL m = showlist [ showvalL a | a <- m ]

showvalL :: ValueL -> String
showvalL WrongL = "<wrong>"
showvalL (NumL i) = show i
showvalL (FunL _) = "<function>"

showlist :: [String] -> String
showlist ss = "[" ++ intercalate ", " ss ++ "]"

testL :: TermL -> String
testL t = showL (interpL t [])

term0L :: TermL
term0L = AppL (LamL "x" (AddL (VarL "x") (VarL "x"))) (AmbL (ConL 1) (ConL 2))

{- | Variation 6: Backwards state -}
data TermB = VarB Name
           | ConB Int
           | AddB TermB TermB
           | LamB Name TermB
           | AppB TermB TermB
           | CountB

data ValueB = WrongB
            | NumB Int
            | FunB (ValueB -> B ValueB)

type EnvironmentB = [(Name, ValueB)]

lookupB :: Name -> EnvironmentB -> B ValueB
lookupB x [] = unitB WrongB
lookupB x ((y, b) : e)
  | x == y    = unitB b
  | otherwise = lookupB x e

interpB :: TermB -> EnvironmentB -> B ValueB
interpB (VarB x) e = lookupB x e
interpB (ConB i) _ = unitB (NumB i)
interpB (AddB u v) e = interpB u e `bindB` (\a ->
                       interpB v e `bindB` (\b ->
                       addB a b))
interpB (LamB x v) e = unitB (FunB (\a -> interpB v ((x, a):e)))
interpB (AppB t u) e = interpB t e `bindB` (\f ->
                       interpB u e `bindB` (\a ->
                       applyB f a))
interpB CountB e = fetchB `bindB` (\i -> unitB (NumB i))

addB :: ValueB -> ValueB -> B ValueB
addB (NumB i) (NumB j) = tickB `bindB` (\() -> unitB (NumB (i+j)))
addB a        b        = unitB WrongB

applyB :: ValueB -> ValueB -> B ValueB
applyB (FunB k) a = tickB `bindB` (\() -> k a)
applyB f        _ = unitB WrongB

type B a = State -> (a, State)

unitB :: a -> B a
unitB a = \s0 -> (a, s0)

bindB :: B a -> (a -> B b) -> B b
m `bindB` k = \s2 -> let (a, s0) = m s1
                         (b, s1) = k a s2
                     in (b, s0)

tickB :: B ()
tickB = \s -> ((), s+1)

fetchB :: B State
fetchB = \s -> (s, s)

showB :: B ValueB -> String
showB m = let (a, s1) = m 0
          in "Value: " ++ showvalB a ++ "; " ++ "Count: " ++ showint s1

showvalB :: ValueB -> String
showvalB WrongB = "<wrong>"
showvalB (NumB i) = show i
showvalB (FunB _) = "<function>"

testB :: TermB -> String
testB t = showB (interpB t [])

term0B :: TermB
term0B = AppB (LamB "x" (AddB (VarB "x") (VarB "x"))) (AddB (ConB 10) (ConB 11))
term1B :: TermB
term1B = AddB (AddB (ConB 1) (ConB 2)) CountB


{- | Variation 0: Standard Interpreter Call-by-Name -}
data ValueI' = WrongI'
             | NumI' Int
             | FunI' (I ValueI' -> I ValueI')

type EnvironmentI' = [(Name, I ValueI')]

interpI' :: TermI -> EnvironmentI' -> I ValueI'
interpI' (VarI x) e = lookupI' x e
interpI' (ConI i) _ = unitI (NumI' i)
interpI' (AddI u v) e = interpI' u e `bindI` (\a ->
                        interpI' v e `bindI` (\b ->
                        addI' a b))
interpI' (LamI x v) e = unitI (FunI' (\a -> interpI' v ((x, a):e)))
interpI' (AppI t u) e = interpI' t e `bindI` (\f ->
                        applyI' f (interpI' u e))

lookupI' :: Name -> EnvironmentI' -> I ValueI'
lookupI' x [] = unitI WrongI'
lookupI' x ((y, b) : e)
  | x == y    = unitI b
  | otherwise = lookupI' x e

applyI' :: ValueI' -> ValueI' -> I ValueI'
applyI' (FunI' k) a = k a
applyI' f         _ = unitI WrongI'

addI' :: ValueI' -> ValueI' -> I ValueI'
addI' (NumI' i) (NumI' j) = unitI (NumI' (i+j))
addI' a        b          = unitI WrongI'

showI' :: I ValueI' -> String
showI' WrongI' = "<wrong>"
showI' (NumI' i) = show i
showI' (FunI' _) = "<function>"

testI' :: TermI -> String
testI' t = showI' (interpI' t [])

term0I' :: TermI
term0I' = AppI (LamI "x" (AddI (VarI "x") (VarI "x"))) (AddI (ConI 10) (ConI 11))


{- | Variation 3: State Call-by-Name -}
data ValueS' = WrongS'
             | NumS' Int
             | FunS' (S ValueS' -> S ValueS')

type EnvironmentS' = [(Name, S ValueS')]

lookupS' :: Name -> EnvironmentS' -> S ValueS'
lookupS' x [] = unitS WrongS'
lookupS' x ((y, b) : e)
  | x == y    = b
  | otherwise = lookupS' x e

interpS' :: TermS -> EnvironmentS' -> S ValueS'
interpS' (VarS x) e = lookupS' x e
interpS' (ConS i) _ = unitS (NumS' i)
interpS' (AddS u v) e = interpS' u e `bindS` (\a ->
                        interpS' v e `bindS` (\b ->
                        addS' a b))
interpS' (LamS x v) e = unitS (FunS' (\a -> interpS' v ((x, a):e)))
interpS' (AppS t u) e = interpS' t e `bindS` (\f ->
                        applyS' f (interpS' u e))
interpS' CountS e = fetchS `bindS` (\i -> unitS (NumS' i))

addS' :: ValueS' -> ValueS' -> S ValueS'
addS' (NumS' i) (NumS' j) = tickS `bindS` (\() -> unitS (NumS' (i+j)))
addS' a        b          = unitS WrongS'

applyS' :: ValueS' -> S ValueS' -> S ValueS'
applyS' (FunS' k) a = tickS `bindS` (\() -> k a)
applyS' f         _ = unitS WrongS'

showS' :: S ValueS' -> String
showS' m = let (a, s1) = m 0
           in "Value: " ++ showvalS' a ++ "; " ++ "Count: " ++ showint s1

showvalS' :: ValueS' -> String
showvalS' WrongS' = "<wrong>"
showvalS' (NumS' i) = show i
showvalS' (FunS' _) = "<function>"

testS' :: TermS -> String
testS' t = showS' (interpS' t [])

term0S' :: TermS
term0S' = AppS (LamS "x" (AddS (VarS "x") (VarS "x"))) (AddS (ConS 10) (ConS 11))

{- | Variation 5: Non-deterministic choice Call-by-Name -}
data ValueL' = WrongL'
             | NumL' Int
             | FunL' (L ValueL' -> L ValueL')

type EnvironmentL' = [(Name, L ValueL')]

lookupL' :: Name -> EnvironmentL' -> L ValueL'
lookupL' x [] = unitL WrongL'
lookupL' x ((y, b) : e)
  | x == y    = b
  | otherwise = lookupL' x e

interpL' :: TermL -> EnvironmentL' -> L ValueL'
interpL' (VarL x) e = lookupL' x e
interpL' (ConL i) _ = unitL (NumL' i)
interpL' (AddL u v) e = interpL' u e `bindL` (\a ->
                        interpL' v e `bindL` (\b ->
                        addL' a b))
interpL' (LamL x v) e = unitL (FunL' (\a -> interpL' v ((x, a):e)))
interpL' (AppL t u) e = interpL' t e `bindL` (\f ->
                        applyL' f (interpL' u e))
interpL' (AmbL u v) e = interpL' u e `plusL` interpL' v e
interpL' FailL _ = zeroL

addL' :: ValueL' -> ValueL' -> L ValueL'
addL' (NumL' i) (NumL' j) = unitL (NumL' (i+j))
addL' a        b          = unitL WrongL'

applyL' :: ValueL' -> L ValueL' -> L ValueL'
applyL' (FunL' k) a = k a
applyL' f         _ = unitL WrongL'

showL' :: L ValueL' -> String
showL' m = showlist [ showvalL' a | a <- m ]

showvalL' :: ValueL' -> String
showvalL' WrongL' = "<wrong>"
showvalL' (NumL' i) = show i
showvalL' (FunL' _) = "<function>"

testL' :: TermL -> String
testL' t = showL' (interpL' t [])

term0L' :: TermL
term0L' = AppL (LamL "x" (AddL (VarL "x") (VarL "x"))) (AmbL (ConL 1) (ConL 2))
