{-# LANGUAGE NPlusKPatterns #-}
module NumberCard where

-- | 3要素のリスト
--  1. カード [1]  はじまりの digit 文字列
--  2. カード [2]  はじまりの digit 文字列
--  3. カード [13] はじまりの digit 文字列
digits :: Int -> [[String]]
digits 0 = [[],[],[""]] -- "" はどこでも良いが1つだけ入れておく
digits 1 = [["1"], ["2"], []]
digits (n+2) = [prefix1', prefix2', prefix13']
  where ([prefix1, prefix2, prefix13], pred2) = (digits (n+1), digits n)
        prefix1'  = map ('1':) $ prefix1 ++ prefix2 ++ prefix13
        prefix2'  = map ('2':) $ prefix1 ++ prefix13
        prefix13' = map ("13"++) $ concat pred2
