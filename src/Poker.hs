module Poker where

type Outs = Int


turnRate :: Outs -> Float
turnRate outs = fromIntegral outs / 47 * 100

riverRate :: Outs -> Float
riverRate outs = (1 - (47 - fromIntegral outs)/47 * (46 - fromIntegral outs)/46) * 100
