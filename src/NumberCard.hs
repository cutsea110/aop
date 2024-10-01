{-# LANGUAGE NPlusKPatterns #-}
module NumberCard where

-- | 3要素のリスト
--  1. カード [1]  はじまりの digit 文字列
--  2. カード [2]  はじまりの digit 文字列
--  3. カード [13] はじまりの digit 文字列
digits :: Integer -> [String]
digits = concat3 . snd . foldn (c, f) . toNat
  where
    concat3 (x, y, z) = x ++ y ++ z
    c = ((["1"], ["2"], []), ([], [], [""]))
    f (pred@(p1, p2, p13), (q1, q2, q3)) = ((p1', p2', p13'), pred)
      where p1'  = map ("1"++) $ p1 ++ p2 ++ p13
            p2'  = map ("2"++) $ p1 ++ p13
            p13' = map ("13"++) $ q1 ++ q2 ++ q3


data Nat = Z | S Nat deriving Show

foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) = u
  where u Z = c
        u (S n) = f (u n)

fromNat :: Nat -> Integer
fromNat = foldn (0, (1+))

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn psi = v
  where v x = case psi x of
          Nothing -> Z
          Just x' -> S (v x')

toNat :: Integer -> Nat
toNat = unfoldn psi
  where psi 0 = Nothing
        psi n = Just (n-1)

digitCount :: Integer -> Integer
digitCount = fst . snd . foldn (c, f) . toNat
  where
    c = ((2, (1, 1, 0)), (1, (0, 0, 0)))
    f (pred@(t1, (p1, p2, p13)), (t2, _)) = ((t1+p1+p13+t2, (t1, p1+p13, t2)), pred)
