module Poker where

import Control.Arrow
import Data.Ratio

type Outs = Integer
type Probability = Ratio Integer
type Odds = Ratio Integer

roundN :: Integer -> Probability -> Probability
roundN n = (/ d) . fromIntegral . round . (* d) where d = 10 ^ n

-- | フロップ時点での outs 数から算出されるターン時の勝率
turnRate :: Outs -> Probability
turnRate outs = outs % 47 * 100

-- | フロップ時点での outs 数から算出されるリバーまで含めた勝率
riverRate :: Outs -> Probability
riverRate outs = (1 - f 47 * f 46) * 100
  where f n = 1 - outs % n

-- | アウツが 1 から 20 のときのターンとリバーまでの勝率
turnsAndRivers :: [(Probability, Probability)]
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

data Hand = Hand { probability :: !Probability
                 , odds :: !Odds
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
numOfStartingHands = 169 -- 13 * 13 == 13 + 78 + 78

numOfPocketPair :: Integer
numOfPocketPair = 13

numOfSuitedHand :: Integer
numOfSuitedHand = 78

numOfOffSuitedHand :: Integer
numOfOffSuitedHand = 78

allPossibleComb :: Integer
allPossibleComb = comb 52 2

calcOdds :: Integer -> Integer -> Odds
calcOdds n k = (n - k) % k

fromProbability :: Probability -> Hand
fromProbability prob = Hand prob odds
  where (n, d) = (numerator prob, denominator prob)
        odds = calcOdds d n

-- b. ポケットペア
-- 数: 13
-- ハンド毎のスーツの組み合わせ: 4C2 = 6
-- 組み合わせ(合計): 13 * 6 = 78
-- 特定のポケットペア P = 6/1326 (オッズ: 1/220)
specificPocketPair :: Hand
specificPocketPair = fromProbability probability
  where probability = suitComb % allPossibleComb
        suitComb = comb 4 2

-- 任意のポケットペア P = 78/1326 (オッズ: 1/16)
anyPocketPair :: Hand
anyPocketPair = fromProbability probability
  where probability = allPocketPairComb % allPossibleComb
        suitComb = comb 4 2
        allPocketPairComb  = numOfPocketPair * suitComb
        

-- c. スーテッドハンド
-- 数: 78
-- ハンド毎のスーツの組み合わせ: 4C1 = 4
-- 組み合わせ(合計): 78 * 4 = 312
-- 特定のスーテッドハンド P = 4/1326 (オッズ: 1/331)
specificSuitedHand :: Hand
specificSuitedHand = fromProbability probability
  where probability = suitComb % allPossibleComb
        suitComb = comb 4 1

-- 任意のスーテッドハンド P = 312/1326 (オッズ: 1/3.25)
anySuitedHand :: Hand
anySuitedHand = fromProbability probability
  where probability = allSuitedHandComb % allPossibleComb
        suitComb = comb 4 1
        allSuitedHandComb = numOfSuitedHand * suitComb

-- d. オフスーツハンド
-- 数: 78
-- ハンド毎のスーツの組み合わせ: 4C1 * 3C1 = 12
-- 組み合わせ(合計): 78 * 12 = 936
-- 特定のオフスーツハンド P = 12/1326 (オッズ: 1/110)
specificOffSuitedHand :: Hand
specificOffSuitedHand = fromProbability probability
  where probability = suitComb % allPossibleComb
        suitComb = comb 4 1 * comb 3 1

-- 任意のオフスーツハンド P = 936/1326 (オッズ: 1/0.417)
anyOffSuitedHand :: Hand
anyOffSuitedHand = fromProbability probability
  where probability = allOffSuitedHandComb % allPossibleComb
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
opponentHasBiggerPair rank = fromProbability probability -- TODO: check rank has to be 2 <= rank <= 14
  where (a, b) = (84 - 6 * rank, 1225)
        probability = a % b

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
opponentsHaveBiggerPairs rank n = fromProbability probability
  where (a, b) = (84 - 6 * rank, 1225)
        probability = p * fromIntegral n - pk
        -- n 人中の誰か 1 人がより大きいペアを持っている確率
        p = (comb n 1 * a) % b
        -- 丁度 i 人のプレイヤーがより大きなポケットペアを持っている確率
        pn i = (a % b)^i
        -- 複数の対戦相手がポケットペアを持っている確率
        pk = sum [ fromIntegral (i-1) * pn i | i <- [2..n]]

-- | 3. 複数の大きいポケットペアに出会う確率
-- 同じ原理で導ける。ただし Pk = P2 + P3 + .. + Pn(2 <= n <= 9)
someManyBiggerPairs :: Rank -> PlayerNum -> Hand
someManyBiggerPairs rank n = fromProbability probability
  where (a, b) = (84 - 6 * rank, 1225)
        probability = p * fromIntegral n - pk
        -- n 人中の誰か 1 人がより大きいペアを持っている確率
        p = (comb n 1 * a) % b
        -- 丁度 i 人のプレイヤーがより大きなポケットペアを持っている確率
        pn i = (a % b)^i
        -- 複数の対戦相手がポケットペアを持っている確率
        pk = sum [pn i | i <- [2..n]]

-- | 4. より良い A に出会う確率
-- a. あなたが Ax ハンドを持っているときに特定の対戦相手が AA を持つ確率
-- 50 毎のカードが残っています(あなたが 2 枚持っていて、その 1 枚が A です)。
-- その中で 3 枚が A です。
-- もし対戦相手の最初のカードが A なら、 49 枚残っているカードのうち 2 枚が彼に AA を与えることができます。
myAxVsAA :: Hand
myAxVsAA = fromProbability probability
  where probability = (3 % 50) * (2 % 49)

-- b. あなたが Ax ハンドを持っているときに特定の対戦相手が AA を持つ確率
-- P = (1-(1-(3/50*2/49)^n)) ~ 0.0218
-- n: 対戦相手の数
myAxVsAnyAA :: PlayerNum -> Hand
myAxVsAnyAA n = fromProbability probability
  where probability = 1 - (1 - ((3 % 50) * (2 % 49)) ^ n)

-- c. あなたが Ax ハンドを持っているときに 1 人の対戦相手がより良い A を持つ確率
-- P = (3/50 * 2/49) + (3/50 * ((13-r)*4)/49 * 2) = (159 - 12r)/1225
-- r はあなたの 2 枚目(キッカー)のカード(2=2, ... J=11, Q=12, K=13) を表します。
myAxVsBetterAx :: Rank -> Hand
myAxVsBetterAx rank = fromProbability probability
  where (a, b) = (159 - 12 * rank, 1225)
        probability = a % b

-- | 5. あなたがポケットペアを持っているときにオーバーカードが現れない確率
-- a. 予備考察
-- 任意のスターティングハンドによる起こり得るフロップ
numOfAllOfFlop :: Integer
numOfAllOfFlop = comb 50 3

-- 任意のスターティングハンドによる起こり得るターン
numOfAllOfTurn :: Integer
numOfAllOfTurn = comb 50 4

-- 任意のスターティングハンドによる起こり得るリバー
numOfAllOfRiver :: Integer
numOfAllOfRiver = comb 50 5

-- b. フロップでオーバーカードが現れない確率
-- r: あなたのポケットペアのランクを表しています(2=2, ... J=11, Q=12, K=13)
-- r より小さいカードは 2..(r-1) の r-2 種類で 4 種類のスーツがあるので 4 * (r-2) 枚。
-- r と同じカードがあなたのペア以外にあと 2 枚残っています。
-- したがってオーバーカードでないカードは 4 * (r-2) + 2 = 4 * r - 6 枚です。
noOverCardAtFlop :: Rank -> Hand
noOverCardAtFlop r = fromProbability probability
  where probability = comb (4 * r - 6) 3 % numOfAllOfFlop

-- c. ターンでオーバーカードが現れない確率
-- r: あなたのポケットペアのランクを表しています(2=2, ... J=11, Q=12, K=13)
-- r より小さいカードは 2..(r-1) の r-2 種類で 4 種類のスーツがあるので 4 * (r-2) 枚。
-- r と同じカードがあなたのペア以外にあと 2 枚残っています。
-- したがってオーバーカードでないカードは 4 * (r-2) + 2 = 4 * r - 6 枚です。
noOverCardAtTurn :: Rank -> Hand
noOverCardAtTurn r = fromProbability probability
  where probability = comb (4 * r - 6) 4 % numOfAllOfTurn

-- d. リバーでオーバーカードが現れない確率
-- r: あなたのポケットペアのランクを表しています(2=2, ... J=11, Q=12, K=13)
-- r より小さいカードは 2..(r-1) の r-2 種類で 4 種類のスーツがあるので 4 * (r-2) 枚。
-- r と同じカードがあなたのペア以外にあと 2 枚残っています。
-- したがってオーバーカードでないカードは 4 * (r-2) + 2 = 4 * r - 6 枚です。
noOverCardAtRiver :: Rank -> Hand
noOverCardAtRiver r = fromProbability probability
  where probability = comb (4 * r - 6) 5 % numOfAllOfRiver

-- | 6. 特定のハンドが配られる確率
numOfAllHands :: Integer
numOfAllHands = comb 52 5

-- a. ロイヤルフラッシュが配られる確率
-- 起こり得るカードの組み合わせは 4C1
royalFlush :: Hand
royalFlush = fromProbability probability
  where probability = comb 4 1 % numOfAllHands

-- b. ストレートフラッシュが配られる確率
-- 起こり得るカードの組み合わせは 10C1 * 4C1 - 4C1
-- 4C1 はロイヤルフラッシュを除いています。
straightFlush :: Hand
straightFlush = fromProbability probability
  where probability = (comb 10 1 * comb 4 1 - comb 4 1) % numOfAllHands

-- c. フォーカードが配られる確率
-- 起こり得るカードの組み合わせは 13C1 * 4C4 * 48C1
fourOfAKind :: Hand
fourOfAKind = fromProbability probability
  where probability = (comb 13 1 * comb 4 4 * comb 48 1) % numOfAllHands

-- d. フルハウスが配られる確率
-- 起こり得るカードの組み合わせは 13C1 * 4C3 * 12C1 * 4C2
fullHouse :: Hand
fullHouse = fromProbability probability
  where probability = (comb 13 1 * comb 4 3 * comb 12 1 * comb 4 2) % numOfAllHands

-- e. フラッシュが配られる確率
-- 起こり得るカードの組み合わせは 13C5 * 4C1 - 10C1 * 4C1
-- 10C1 * 4C1 はストレートフラッシュとロイヤルストレートを除いています。
flush :: Hand
flush = fromProbability probability
  where probability = (comb 13 5 * comb 4 1 - comb 10 1 * comb 4 1) % numOfAllHands
  

-- f. ストレートが配られる確率
-- 起こり得るカードの組み合わせは 10C1 * 4C1^5 - 10C1 * 4C1
-- 10C1 * 4C1 はストレートフラッシュとロイヤルストレートを除いています。
straight :: Hand
straight = fromProbability probability
  where probability = (comb 10 1 * comb 4 1 ^ 5 - comb 10 1 * comb 4 1) % numOfAllHands

-- g. スリーカードが配られる確率
-- 起こり得るカードの組み合わせは 13C1 * 4C3 * 12C2 * 4C1^2
threeOfAKind :: Hand
threeOfAKind = fromProbability probability
  where probability = (comb 13 1 * comb 4 3 * comb 12 2 * comb 4 1 ^ 2) % numOfAllHands

-- h. ツーペアが配られる確率
-- 起こり得るカードの組み合わせは 13C2 * 4C2^2 * 11C1 * 4C1
twoPair :: Hand
twoPair = fromProbability probability
  where probability = (comb 13 2 * comb 4 2 ^ 2 * comb 11 1 * comb 4 1) % numOfAllHands

-- i. ワンペアが配られる確率
-- 起こり得るカードの組み合わせは 13C1 * 4C2 * 12C3 * 4C1^3
onePair :: Hand
onePair = fromProbability probability
  where probability = (comb 13 1 * comb 4 2 * comb 12 3 * comb 4 1 ^ 3) % numOfAllHands

-- j. ハイカードが配られる確率
-- 起こり得るカードの組み合わせは (13C5 - 10) * (4C1^5 - 4C1)
-- 数の組み合わせは13 種類のうち 5 種類を選ぶが、ストレートになる 10 通りは除く。
-- スートは 4 種類 5 枚分の組み合わせからフラッシュになる 4 通りは除く。
highCard :: Hand
highCard = fromProbability probability
  where probability = ((comb 13 5 - 10) * (comb 4 1 ^ 5 - comb 4 1)) % numOfAllHands

-- | 7. フロップで良くなる確率
-- x: プリフロップでのあなたのハンドのアウツの数
-- y: アウツのうちの何枚をヒットしたいか
-- a: アウツではない残りのカード枚数
-- b: a からヒットしたいカード枚数
-- y + b がフロップの 3 枚となることに注意
--
-- 例3: ポケットペアはフロップで(ちょうど)スリーカードに次の確率でなる。
-- 2C1 * 48C2 / 50C3
-- アウツは 2 で、そのうちの 1 毎を hit したいので 2C1
-- アウツを除くと残りは 48 枚で、その中から 2 枚を選ぶので 48C2
--
-- 例4: 2 枚のスーテッドカードはフロップで完成したフラッシュに次の確率でなる。
-- 11C3 * 39C0 / 50C3
-- アウツは同じスートの残りカード 11 枚で、その中から 3 枚を hit したいので 11C3
-- アウツを除くと残りは 50-11=39 枚で、その中から 0 枚を選ぶので 39C0 = 1
--
flopMakeHandBetter :: Outs -> Integer -> Hand
flopMakeHandBetter outs hit = fromProbability probability
  where (x, y) = (outs, hit)
        (a, b) = (52 - 2 - x, 3 - y)
        probability = comb x y * comb a b % numOfAllOfFlop

-- | 8. ターンで良くなる確率
turnMakeHandBetter :: Outs -> Hand
turnMakeHandBetter outs = fromProbability probability
  where probability = outs % 47

-- | 9. リバーで良くなる確率
riverMakeHandBetter :: Outs -> Hand
riverMakeHandBetter outs = fromProbability probability
  where probability = outs % 46
