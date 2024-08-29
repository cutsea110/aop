module Poker where

import Control.Arrow
import Data.Ratio

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

-- | https://ja.pokerstrategy.com/strategy/various-poker/%E3%83%86%E3%82%AD%E3%82%B5%E3%82%B9%E3%83%9B%E3%83%BC%E3%83%AB%E3%83%87%E3%83%A0-%E7%A2%BA%E7%8E%87/
--
fact :: Integer -> Integer
fact n = product [1..n]

perm :: Integer -> Integer -> Integer
perm n k = fact n `div` fact (n - k)

comb :: Integer -> Integer -> Integer
comb n k = perm n k `div` fact k

data Hand = Hand { probability :: Float
                 , odds :: Ratio Integer
                 }
            deriving (Show)

-- | 1. 特定のスターティングハンドが配られる確率
-- a. 予備考察
-- スターティングハンドの数: 169
-- 内訳
-- - ポケットペア: 13
-- - スーテッドハンド: 78
-- - オフスーツハンド(ポケットを除く): 78
-- 起こり得る全ての組み合わせ: 52C2 = 1326
--
numOfStartingHands :: Integer
numOfStartingHands = 169

numOfPocketPair :: Integer
numOfPocketPair = 13

numOfSuitedHand :: Integer
numOfSuitedHand = 78

numOfOffSuitedHand :: Integer
numOfOffSuitedHand = 78

allPossibleComb :: Integer
allPossibleComb = comb 52 2

calcOdds :: Integer -> Integer -> Ratio Integer
calcOdds n k = (n - k) % k

-- b. ポケットペア
-- 数: 13
-- ハンド毎のスーツの組み合わせ: 4C2 = 6
-- 組み合わせ(合計): 13 * 6 = 78
-- 特定のポケットペア P = 6/1326 (オッズ: 1/221)
specificPocketPair :: Hand
specificPocketPair = Hand probability odds
  where probability = fromIntegral suitComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb suitComb
        suitComb = comb 4 2

-- 任意のポケットペア P = 78/1326 (オッズ: 1/17)
anyPocketPair :: Hand
anyPocketPair = Hand probability odds
  where probability = fromIntegral allPocketPairComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb allPocketPairComb
        suitComb = comb 4 2
        allPocketPairComb  = numOfPocketPair * suitComb
        

-- c. スーテッドハンド
-- 数: 78
-- ハンド毎のスーツの組み合わせ: 4C1 = 4
-- 組み合わせ(合計): 78 * 4 = 312
-- 特定のスーテッドハンド P = 4/1326 (オッズ: 1/331)
specificSuitedHand :: Hand
specificSuitedHand = Hand probability odds
  where probability = fromIntegral suitComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb suitComb
        suitComb = comb 4 1

-- 任意のスーテッドハンド P = 312/1326 (オッズ: 1/4)
anySuitedHand :: Hand
anySuitedHand = Hand probability odds
  where probability = fromIntegral allSuitedHandComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb allSuitedHandComb
        suitComb = comb 4 1
        allSuitedHandComb = numOfSuitedHand * suitComb
