module Poker where

type Outs = Int

pair :: (a -> b) -> (a, a) -> (b, b)
pair f (x, y) = (f x, f y)

-- | フロップ時点での outs 数から算出されるターン時の勝率
turnRate :: Outs -> Float
turnRate outs = fromIntegral outs / 47 * 100

-- | フロップ時点での outs 数から算出されるリバーまで含めた勝率
riverRate :: Outs -> Float
riverRate outs = (1 - f 47 * f 46) * 100
  where
    f n = 1 - fromIntegral outs / n

-- | アウツが 1 から 20 のときのターンとリバーまでの勝率
turnsAndRivers :: [(Float, Float)]
turnsAndRivers = map (\i -> pair round2 (turnRate i, riverRate i)) [1..20]
  where
    round2 :: Float -> Float
    round2 = (/ 100) . fromIntegral . round . (* 100)
