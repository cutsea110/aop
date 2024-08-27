module Poker where

import Control.Arrow

type Outs = Int

roundN :: Int -> Float -> Float
roundN n = (/ d) . fromIntegral . round . (* d) where d = 10 ^ n

-- | フロップ時点での outs 数から算出されるターン時の勝率
turnRate :: Outs -> Float
turnRate outs = fromIntegral outs / 47 * 100

-- | フロップ時点での outs 数から算出されるリバーまで含めた勝率
riverRate :: Outs -> Float
riverRate outs = (1 - f 47 * f 46) * 100
  where f n = 1 - fromIntegral outs / n

-- | アウツが 1 から 20 のときのターンとリバーまでの勝率
turnsAndRivers :: [(Float, Float)]
turnsAndRivers = map (round2 . turnRate &&& round2 . riverRate) [1..20]
  where round2 = roundN 2
