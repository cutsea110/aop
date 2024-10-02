{-# LANGUAGE NPlusKPatterns #-}
module NumberCard where

import Data.Monoid (Sum(..))

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

gdigits :: Monoid a
        => (a, a, a) -- 0th element
        -> (a, a, a) -- 1st element
        -> (a -> a)  -- next element starting with 1
        -> (a -> a)  -- next element starting with 2
        -> (a -> a)  -- next element starting with 13
        -> Int -> (a, a, a)
gdigits r0 r1 f1 f2 f13 = u
  where
    u = (map f [0..] !!)
    f 0 = r0
    f 1 = r1
    f n = (p1', p2', p13')
      where
        (p1, p2, p13) = u (n-1)
        (q1, q2, q13) = u (n-2)
        p1'  = f1  $! p1<>p2<>p13
        p2'  = f2  $! p1<>p13
        p13' = f13 $! q1<>q2<>q13


-- | NOTE: 0番目の要素は p13 に [""] を入れたけどどこでも良い
digits :: Int -> ([] String, [] String, [] String)
digits = gdigits ([], [], [""]) (["1"], ["2"], []) (map ("1"<>)) (map ("2"<>)) (map ("13"<>))

-- | NOTE: 0番目の要素は p13 に Sum 1 を入れたけどどこでも良い
digitCount :: Int -> (Sum Integer, Sum Integer, Sum Integer)
digitCount = gdigits (Sum 0, Sum 0, Sum 1) (Sum 1, Sum 1, Sum 0) id id id
