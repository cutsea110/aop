{-# LANGUAGE NPlusKPatterns #-}
module NumberCard where

import Data.Monoid ((<>))

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

data Nat = Z | S Nat deriving Show

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn psi = v
  where
    v x = case psi x of
      Nothing -> Z
      Just y  -> S (v y)

data NEL a = Unit a | Cons a (NEL a) deriving Show
extract :: NEL a -> a
extract (Unit x)   = x
extract (Cons x _) = x

histo :: (c, NEL c -> c) -> Nat -> c
histo (c, f) = extract . u
  where
    u Z     = Unit c
    u (S n) = let n' = u n in Cons (f n') n'

dyna :: (c, NEL c -> c) -> (a -> Maybe a) -> a -> c
dyna phi psi = histo phi . unfoldn psi

-- by using dyna
calc :: Int -> [String]
calc = into . dyna (c, f) psi
  where
    c          = ([], [], [""])
    f (Unit _) = (["1"], ["2"], [])
    f (Cons (x1, y1, z1) (Unit (x2, y2, z2))) = (x', y', z')
      where x' = map ("1"<>) (x1<>y1<>z1)
            y' = map ("2"<>) (x1<>z1)
            z' = map ("13"<>) (x2<>y2<>z2)
    f (Cons (x1, y1, z1) (Cons (x2, y2, z2) _)) = (x', y', z')
      where x' = map ("1"<>) (x1<>y1<>z1)
            y' = map ("2"<>) (x1<>z1)
            z' = map ("13"<>) (x2<>y2<>z2)

    psi 0     = Nothing
    psi (n+1) = Just n

    into (x, y, z) = x<>y<>z

-- | solution
solve :: Int -> Int
solve = length . calc

-- efficient version
solve' :: Int -> Int
solve' = fst . dyna (c, f) psi
  where
    -- (すべての組み合わせの数, 2始まりを除外した組み合わせの数)
    c          = (0, 0)
    f (Unit _) = (2, 1)
    f (Cons (t1, w1) (Unit (t2, _)))    = (t1+w1+ 1, t1+ 1)
    f (Cons (t1, w1) (Cons (t2, w2) _)) = (t1+w1+t2, t1+t2)

    psi 0     = Nothing
    psi (n+1) = Just n
