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
-- 特定のポケットペア P = 6/1326 (オッズ: 1/220)
specificPocketPair :: Hand
specificPocketPair = Hand probability odds
  where probability = fromIntegral suitComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb suitComb
        suitComb = comb 4 2

-- 任意のポケットペア P = 78/1326 (オッズ: 1/16)
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

-- 任意のスーテッドハンド P = 312/1326 (オッズ: 1/3.25)
anySuitedHand :: Hand
anySuitedHand = Hand probability odds
  where probability = fromIntegral allSuitedHandComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb allSuitedHandComb
        suitComb = comb 4 1
        allSuitedHandComb = numOfSuitedHand * suitComb

-- d. オフスーツハンド
-- 数: 78
-- ハンド毎のスーツの組み合わせ: 4C1 * 3C1 = 12
-- 組み合わせ(合計): 78 * 12 = 936
-- 特定のオフスーツハンド P = 12/1326 (オッズ: 1/110)
specificOffSuitedHand :: Hand
specificOffSuitedHand = Hand probability odds
  where probability = fromIntegral suitComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb suitComb
        suitComb = comb 4 1 * comb 3 1

-- 任意のオフスーツハンド P = 936/1326 (オッズ: 1/0.417)
anyOffSuitedHand :: Hand
anyOffSuitedHand = Hand probability odds
  where probability = fromIntegral allOffSuitedHandComb / fromIntegral allPossibleComb
        odds = calcOdds allPossibleComb allOffSuitedHandComb
        suitComb = comb 4 1 * comb 3 1
        allOffSuitedHandComb = numOfOffSuitedHand * suitComb

--
-- TODO: e. 範囲
--


-- | 2. ポケットペアを持っているときにより大きいポケットペアに出会う確率
-- a. 1人の対戦相手のペアがより大きい確率
-- P = (14 - r) * 4/50 * 3/49 = (84 - 6r)/1225
-- r: 自分のペアのランク (2=2,...J=11,Q=12,K=13,A=14)
-- (14-r)*4 枚のより大きいカードがあります。対戦相手は残りの 50 枚のカードのどれでも持つことができます。
-- (あなたは他の 2 枚を持っています)
-- もし相手の最初のカードがあなたのポケットペアより大きい場合、残りの 49 毎のカードのうち 3 枚が相手により大きいペアを完成させます。
type Rank = Integer

opponentHasBiggerPair :: Rank -> Hand
opponentHasBiggerPair rank = Hand probability odds -- TODO: check rank has to be 2 <= rank <= 14
  where (a, b) = (84 - 6 * rank, 1225)
        probability = fromIntegral a / fromIntegral b
        odds = calcOdds b a

-- b. 複数の対戦相手の 1 人がより大きいペアを持っている確率
-- まず 1 人の対戦相手がより大きいペアを持っている確率をハンドに残っているプレイヤーの数で掛けます(n)。
-- そして 1 人より多くの対戦相手がより大きいペアを持っている確率を引きます。(Pk)
-- P = ((84 - 6r) / 1225) * n - Pk
-- n: ハンドに残っているプレイヤー数
-- Pk: 複数の対戦相手がポケットペアを持っている確率
-- Pk = P2 + 2*P3 + 3*P4 + ... + (n-1)*Pn
-- Pn = 丁度 n 人のプレイヤーがポケットペアを持っている確率(2 <= n <= 9)
type PlayerNum = Integer

-- | TODO: 確信がないので検証要
opponentsHaveBiggerPairs :: Rank -> PlayerNum -> Hand
opponentsHaveBiggerPairs rank n = Hand probability odds
  where (a, b) = (84 - 6 * rank, 1225)
        probability = p * fromInteger n - pk
        odds = probToOdds probability
        -- n 人中の誰か 1 人がより大きいペアを持っている確率
        p = fromIntegral (comb n 1 * a) / fromIntegral b
        -- 丁度 i 人のプレイヤーがより大きなポケットペアを持っている確率
        pn i = (fromIntegral a / fromIntegral b)^i
        -- 複数の対戦相手がポケットペアを持っている確率
        pk = sum [ fromIntegral (i-1) * pn i | i <- [2..n]]

probToOdds :: Float -> Ratio Integer
probToOdds p = calcOdds b a
  where r = toRational p
        (a, b) = (numerator r, denominator r)


-- | 3. 複数の大きいポケットペアに出会う確率
-- 同じ原理で導ける。ただし Pk = P2 + P3 + .. + Pn(2 <= n <= 9)
someManyBiggerPairs :: Rank -> PlayerNum -> Hand
someManyBiggerPairs rank n = Hand probability odds
  where (a, b) = (84 - 6 * rank, 1225)
        probability = p * fromInteger n - pk
        odds = probToOdds probability
        -- n 人中の誰か 1 人がより大きいペアを持っている確率
        p = fromIntegral (comb n 1 * a) / fromIntegral b
        -- 丁度 i 人のプレイヤーがより大きなポケットペアを持っている確率
        pn i = (fromIntegral a / fromIntegral b)^i
        -- 複数の対戦相手がポケットペアを持っている確率
        pk = sum [pn i | i <- [2..n]]

-- | より良い A に出会う確率
-- a. あなたが Ax ハンドを持っているときに特定の対戦相手が AA を持つ確率
-- 50 毎のカードが残っています(あなたが 2 枚持っていて、その 1 枚が A です)。
-- その中で 3 枚が A です。
-- もし対戦相手の最初のカードが A なら、 49 枚残っているカードのうち 2 枚が彼に AA を与えることができます。
opponentHasAAmyHandAx :: Hand
opponentHasAAmyHandAx = Hand probability odds
  where probability = 6 / 2450 -- 3 / 50 * 2 / 49
        odds = calcOdds 2450 6
