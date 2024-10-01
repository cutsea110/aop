{-# LANGUAGE NPlusKPatterns #-}
module NumberCard where

import Data.List (mapAccumL)

------------------------------------------------------
-- | 2024年度 豊島岡女子中学校 第一回入試問題 第4問
------------------------------------------------------
-- | 3種類のカード [1], [2], [13] がそれぞれたくさんあります。
--   これらのカードを [2] のカードが連続しないように並べて整数を作ります。
--   例えば,
--   1桁の整数は [1], [2] の 2 通り。
--   2桁の整数は [1][1], [1][2], [2][1], [13] の 4 通り。
--   3桁の整数は [1][1][1], [1][1][2], [1][2][1], [1][13], [2][1][1], [2][1][2],
--   [2][13], [13][1], [13][2]の 9 通り作ることができます。
--   このとき, 次の各問に答えなさい。
--
--   (1) 4 桁の整数は何通り作ることができますか。
--   (2) 6 桁の整数は何通り作ることができますか。


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

digitCount' :: Int -> (Integer, (Integer, Integer, Integer))
digitCount' = (map d [0..] !!)
  where
    d 0 = (1, (0, 0, 0))
    d 1 = (2, (1, 1, 0))
    d n = (t1+p1+p13+t2, (t1, p1+p13, t2))
      where
        (t1, (p1, p2, p13)) = digitCount' (n-1)
        (t2, _) = digitCount' (n-2)
