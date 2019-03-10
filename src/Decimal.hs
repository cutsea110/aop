module Decimal where

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show, Eq)
data DigitPlus = DP1 | DP2 | DP3 | DP4 | DP5 | DP6 | DP7 | DP8 | DP9 deriving (Show, Eq)
data Decimal = Wrap DigitPlus | Snoc Decimal Digit deriving (Show, Eq)

foldDec :: (DigitPlus -> t, (t, Digit) -> t) -> Decimal -> t
foldDec (f, g) = u
  where
    u (Wrap dp) = f dp
    u (Snoc dc d) = g (u dc, d)

unfoldDec :: (t -> Either DigitPlus (t, Digit)) -> t -> Decimal
unfoldDec psi = v
  where
    v dc = case psi dc of
      Left dp       -> Wrap dp
      Right (dc', d) -> Snoc (v dc') d

data Nat = Z | S Nat deriving (Show, Eq)

foldn :: (t, t -> t) -> Nat -> t
foldn (c, f) = u
  where
    u Z     = c
    u (S x) = f (u x)

unfoldn :: (t -> Maybe t) -> t -> Nat
unfoldn psi = v
  where
    v x = case psi x of
      Nothing -> Z
      Just x' -> S (v x')

paran (c, f) = u
  where
    u Z = c
    u (S n) = f (u n, n)

hylo :: Functor f => (f b -> b, a -> f a) -> a -> b
hylo (phi, psi) = phi . fmap (hylo (phi, psi)) . psi

plusN x = foldn (x, S)
multN x = foldn (Z, plusN x)
exprN x = foldn (S Z, multN x)
fact = paran (S Z, f)
  where
    f (m, n) = multN m (S n)

x `minusN` y = go x y Z
  where
    go x Z n = Just x
    go Z y n = Nothing
    go (S x) (S y) n = go x y (S n)

succ' Z     = Nothing
succ' (S n) = Just n

{--
x `divN` y = unfoldn phi x
  where
    phi x = x `minusN` y
--}

x `divN` y = hylo (phi, psi) x
  where
    phi (Left n) = Z
    phi (Right x) = S x
    psi x = case x `minusN` y of
      Nothing -> Left x
      Just x' -> Right x'

x `modN` y = hylo (phi, psi) x
  where
    phi (Left n) = n
    phi (Right x) = x
    psi x = case x `minusN` y of
      Nothing -> Left x
      Just x' -> Right x'

{--
x `modN` y = case x `minusN` y of
  Nothing -> x
  Just x' -> x' `modN` y
--}

fromNat = foldn (0, (1+))
toNat = unfoldn phi
  where
    phi x | x <= 0 = Nothing
          | otherwise = Just (x-1)

data NatPlus = One | Spl NatPlus deriving (Show, Eq)

foldnp (c, f) = u
  where
    u One = c
    u (Spl x) = f (u x)

unfoldnp psi = v
  where
    v x = case psi x of
      Nothing -> One
      Just x' -> Spl (v x')

plusNP x = foldnp (Spl x, Spl)
multNP x = foldnp (x, plusNP x)
exprNP x = foldnp (x, multNP x)

fromNatPlus = foldnp (1, (1+))
toNatPlus = unfoldnp phi
  where
    phi x | x <= 1 = Nothing
          | otherwise = Just (x-1)

embed :: DigitPlus -> NatPlus
embed DP1 = One
embed DP2 = Spl (embed DP1)
embed DP3 = Spl (embed DP2)
embed DP4 = Spl (embed DP3)
embed DP5 = Spl (embed DP4)
embed DP6 = Spl (embed DP5)
embed DP7 = Spl (embed DP6)
embed DP8 = Spl (embed DP7)
embed DP9 = Spl (embed DP8)

val = foldDec (embed, op)

op :: (NatPlus, Digit) -> NatPlus
op (n, d) = add (multNP n10 n) d
  where
    n10 = toNatPlus 10
    add n D0 = n
    add n D1 = Spl n
    add n D2 = Spl (add n D1)
    add n D3 = Spl (add n D2)
    add n D4 = Spl (add n D3)
    add n D5 = Spl (add n D4)
    add n D6 = Spl (add n D5)
    add n D7 = Spl (add n D6)
    add n D8 = Spl (add n D7)
    add n D9 = Spl (add n D8)

divNP :: NatPlus -> NatPlus -> NatPlus
x `divNP` y = undefined

modNP :: NatPlus -> NatPlus -> NatPlus
modNP = undefined

npToDigit :: NatPlus -> Digit
npToDigit = undefined

op' :: NatPlus -> (NatPlus, Digit)
op' np = (np', d)
  where
    n10 = toNatPlus 10
    np' = np `divNP` n10
    d   = npToDigit $ np `modNP` n10
