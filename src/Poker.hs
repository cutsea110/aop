module Poker where

type Outs = Int

pair :: (a -> b) -> (a, a) -> (b, b)
pair f (x, y) = (f x, f y)

turnRate :: Outs -> Float
turnRate outs = fromIntegral outs / 47 * 100

riverRate :: Outs -> Float
riverRate outs = (1 - f 47 * f 46) * 100
  where
    f n = 1 - fromIntegral outs / n


turnsAndRivers :: [(Float, Float)]
turnsAndRivers = map (\i -> pair round2 (turnRate i, riverRate i)) [1..20]
  where
    round2 :: Float -> Float
    round2 = (/ 100) . fromIntegral . round . (* 100)
