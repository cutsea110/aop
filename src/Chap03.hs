{-# LANGUAGE NPlusKPatterns, TypeOperators #-}
module Chap03 where

import Prelude hiding (foldr, sum, product, length, round)
import GHC.Int

-- | Ex 3.1
--
-- FX = 1 + N * X
--
-- <[zero, plus] . Foutl, [zero, succ . outr] . Foutr> = [zeross, pluss] を示す.
--
foldr (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

sum :: Num a => [a] -> a
sum = foldr (zero, plus)
  where zero = 0
        plus (x, y) = x + y

length = foldr (zero, succ . outr)
  where zero = 0
        outr (x, y) = y
        succ n = 1 + n

average = div' . pair (sum, length)
  where
    pair (f, g) x = (f x, g x)
    div' (x, y) = x / y

avg = div' . foldr (zeross, pluss)
      where zeross = (0, 0)
            pluss (a, (b, n)) = (a + b, n + 1)
            div' (x, y) = x / y
--
--  <[zero, plus] . Foutl, [zero, succ . outr] . Foutr>
-- ==
--  <[zero, plus] . (id + id * outl), [zero, succ . outr] . (id + id * outr)>
-- ==
--  <[zero, plus . (id, outl)], [zero, succ . outr . (id * outr)]>
-- ==
--  [<zero, zero>, <plus . (id * outl), succ . outr . (id * outr)>]
--
-- 第2成分をポイントワイズに解くと
--  <plus . (id * outl), succ . outr . (id * outr)> (a, (b, n))
-- ==
--  (plus . (id * outl) (a, (b, n)), succ . outr . (id * outr) (a, (b, n)))
-- ==
--  (plus (a, b), succ (outr (a, n)))
-- ==
--  (a+b, 1+n)
--
-- | Ex 3.2
--
-- <Foutl, Foutr> : FA * FB <- F(A * B)
--
--                 <Foutl, Foutr>
--      FA * FB <----------------- F(A * B)
--         |                           |
-- Fh * Fk |                           |  F(h * k)
--         |                           |
--         v                           v
--      FC * FD <----------------- F(C * D)
--                 <Foutl, Foutr>
--
--
--  (Fh * Fk) . <Foutl, Foutr>
-- == {- 積の吸収則 (2.8) -}
--  <Fh . Foutl, Fk . Foutr>
-- == {- 下図 -}
--  <Foutl . F(h * k), Foutr . F(h * k)>
-- == {- 積の融合則 (2.6) -}
--  <Foutl, Foutr> . F(h * k)
--
--  Fh . Foutl = Foutl . F(h * k)   Fk . Foutr = Foutr . F(h * k)
--
--       Foutl                         Foutr
--   FA <----- F(A * B)            FB <----- F(A * B)
--    |            |               |             |
--  Fh|            | F(h * k)    Fk|             | F(h * k)
--    v            v               v             v
--   FC <----- F(C * D)            FD <----- F(C * D)
--       Foutl                         Foutr
--
-- | Ex 3.3
--
naiveSteep [] = True
naiveSteep (a:x) = a > sum x && naiveSteep x

--            [nil, cons]
--         TB <-------- 1 + B * TB
--        /|                |
--       / |                |
-- steep/  |                |
--     /   |(|c,f|)         |1 + 1 * <steep, sum>
--    /    | = <steep,sum>  |
--   /     |                |
--  v      |                |
--  A <- A * B <-------- 1 + B * (A * B)
--    outl        [c, f]
--
steep = outl . foldr (c, f)
  where outl (x, _) = x
        c = (True, 0)
        f (a, (b, x)) = (a > x && b, a + x)

test_naiveSteep = naiveSteep $ map (2^) ([5000,4999..0] :: [Integer])
test_steep = steep $ map (2^) ([5000,4999..0] :: [Integer])

-- | Ex 3.4
--
--         a
--   T <------- FT
--   |           |
--  f|           |F<f, (|h|)>
--   |           |
--   v           v
--   A <------- F(A * B)
--         g
--
--                           a
--            T <------------------------- FT
--           /|\                           /|\
--          / | \                         / | \
--         /  |  \                       /  |  \
--       f/   |   \                   Ff/   |   \
--       /  (|k|)  \ (|h|)             /  F(|k|) \F(|h|)
--      /     =     \                 /     =     \
--     /   <f,(|h|)> \               /  F<f,(|h|)> \
--    v       v       v             v       v       v
--   A <--- A * B ---> B          FA <- F(A * B) -> FB
--   ^  outl  ^        ^            Foutl  /  Foutr /
--    \        \        \__________________________/
--     \        \                   h    /
--      \        \_____________________ /
--       \                 k           /
--        \___________________________/
--                       g
--
-- k の定義
--   k : A * B <- F(A * B)
--   k = <g, h . Foutr>
--
-- cata の普遍性から
-- (|k|) = <f, (|h|)> == <f, (|h|)> . a = k . F<f, (|h|)>
--
--  k . F<f, (|h|)>
-- == {- k の定義 -}
--  <g, h . Foutr> . F<f, (|h|)>
-- == {- 対の融合 -}
--  <g . F<f, (|h|)>, h . Foutr . F<f, (|h|)>>>
-- == {- 関手則および対の消去則 -}
--  <g . F<f, (|h|)>, h . F(|h|)>
-- == {- 所与の図式および cata -}
--  <f . a, (|h|) . a>
-- == {- 対の融合 -}
--  <f, (|h|)> . a
--
-- | Ex 3.5
--
data Tree a = Null
            | Node (Tree a, a, Tree a)
            deriving Show
--
-- balanced
--    x 1/3 <= n/(n+m+1) <= 2/3
--    o n + 1 <= 3(m + 1) /\ m + 1 <= 3(n + 1)
--
--       [null, node]
--  Ta <------------- 1 + Ta * a * Ta
--  |                        |
-- u|                        | 1 + u * 1 * u
--  v                        v
--  X  <------------- 1 + X  * a * X
--         [c, f]

foldt :: (a, (a, b, a) -> a) -> Tree b -> a
foldt (c, f) = u
  where u Null = c
        u (Node (l, x, r)) = f (u l, x, u r)

size :: Fractional b => Tree a -> b
size = foldt (c, f)
  where c = 1 -- point!
        f (n, a, m) = n + 1 + m

naiveBalanced :: Tree a -> Bool
naiveBalanced Null = True
naiveBalanced (Node (x, a, y)) = balanced x && balanced y &&
                                 1/3 <= v && v <= 2/3
  where (n, m) = (size x, size y)
        v = n / (n + m + 1)

balanced :: Tree a -> Bool
balanced = outl . foldt (c, f)
  where outl (x, _) = x
        c = (True, 1)
        f ((b, n), a, (c, m)) = (b && c && 1/3 <= v && v <= 2/3, n + 1 + m)
          where v = n / (n + m + 1)

-- | Ex 3.6
--
foldn (c, f) 0 = c
foldn (c, f) (n+1) = f (foldn (c, f) n)

preds = outl . foldn (c, f)
  where outl (x, _) = x
        c = ([], 0)
        f (ns, n) = (n+1:ns, n+1)

-- | Ex 3.7
--
product = foldr (c, f)
  where c = 1
        f (n, m) = n * m

-- fact = product . preds
fact = outl . foldn (c, f)
  where outl (x, _) = x
        c = (1, 0)
        f (m, n) = ((n+1)*m, n+1)

-- | Ex 3.8
--
--            a                                 a
--   T <-------------- FT              T <-------------- FT
--   |                /|\              |                /|\
--   |             Ff/ | \Fg           |             Ff/ | \Fg
--   |              /  |  \            |              /  |  \
--   |             v   |   v           |             v   |   v  
--  f|            FA F<f,g> FB        g|            FA F<f,g> FB
--   |             ^   |   ^           |             ^   |   ^
--   |         Foutl\  |  /Foutr       |         Foutl\  |  /Foutr
--   |               \ | /             |               \ | /
--   |                \v/              |                \v/
--   A <----------- F(A x B)           A <----------- F(A x B)
--            h                                 k
-- 上図から
--
--                     a
--         T <-------------------- FT
--        /|\                      /|\
--      f/ | \g                 Ff/ | \Fg
--      /  |  \                  /  |  \
--     v   |   v                v   |   v
--     A <f,g> B               FA F<f,g> FB
--     ^   |   ^                ^   |   ^
--      \  |  /                  \  |  /
--   outl\ | /outr           Foutl\ | /Foutr
--        \v/                      \v/
--       A x B <--------------- F(A x B)
--                   <h,k>
--
-- <f,g> = (|<h,k>|)
--
-- バナナスプリット則
--   f := (|f|)      g := (|g|)
--   h := f . Foutl  k := g . Foutr
-- と置き換えると
-- <(|f|), (|g|)> = (|<f . Foutl, g . Foutr>|)
--
--  Ex 3.4
--   f := f g := (|h|)
--   h := g k := h . Foutr
--  と置き換えると
--  <f, (|h|)> = (|<g, h . Foutr>|)
--
-- | Ex 3.9
--
tri f = foldr (c, g)
  where c = []
        g (x, xs) = x:map f xs

slice = tri tail

-- | Ex 3.10
--
bhp = prod . tri sqr
prod = foldr (1, mul)
mul (x, y) = x * y
sqr x = x^2

--  bhp
-- == {- bhp の定義 -}
--  prod . tri sqr
-- == {- prod の定義 -}
--  (|1, mul|) . tri sqr
-- == {- 後述 -}
--  (|1, mul . (id x sqr)|)
--
--
-- 上記の後述とあるステップは,
--
-- ホーナー則
--  (|c, g|) . tri f = (|c, g . (id x f)|) <= f . c = c かつ f . g = g . (f x f)
--
--  (|1, mul|) . tri sqr のケースを考える
--  c := 1, g := mul, f := sqr と置くと
--  sqr . 1 = 1 これは sqr 1 = 1 なので真
--  sqr . mul = mul . (sqr x sqr) これも sqr (mul (a, b)) = sqr (a * b) = sqr a * sqr b = mul (sqr x sqr) (a, b) なので真
--  よって, (|1, mul|) . tri sqr = (|1, mul . (id x sqr)|) とできる

bhp' = foldr (c, f)
  where c = 1
        f (a, b) = mul (a, sqr b)

-- | Ex 3.11
--
-- tri の定義
--  tri :: (a -> a) -> Ta -> Ta
--  tri f = (|a . F(id, Tf)|)
--
-- 命題
--   (|g|) . tri f = (|g . F(id, h)|) <= h . g = g . F(f, h) 
--
-- これは tri f = (|a . F(id, Tf)|) と cata で書けるので融合則(2.12)
--
--   h . (|f|) = (|g|) <= h . f = g . Fh
--
-- に当てはめることができる.
-- 分かりやすくするために命題の tri f を置き換えて
--
--   (|g|) . (|a . F(id, Tf)|) = (|g . F(id, h)|)
--   ~~~~~     ~~~~~~~~~~~~~       ~~~~~~~~~~~~
--     h           f                     g
--
-- と対応を取ればよいので満たすべき融合則の条件式
--   h . f = g . Fh (これは融合則 (2.12) の条件式)
-- は
--   h . f = g . F(id, h)
-- であることに注意すると
--
--   (|g|) . a . F(id, Tf) = g . F(id, h) . F(id, (|g|))
--
-- となる.つまりこの条件を満たせば命題の主張の結言を言える.
-- したがって,この条件式が命題の前言と同じであることを証明すれば良い?
--
-- | Ex 3.12
--
-- tri f [a0, a1 .. an] = [a0, f a1 .. f^n an]
--
-- なので多項式評価は
--
-- ( (|[0, (+)]|) . tri f ) [a0, a1 .. an] = a0 + f a1 + f^2 a2 + .. f^n an
--                                         = a0 + (*x) a1 + ((*x) . (*x)) a2 ... ((*x) . (*x) .. (*x)) an
--                                         = a0 + a1*x + a2*x*x + a3*x*x*x + .. an*x*x..*x
--                                         = a0 + (a1 + (a2 + .. (an + 0) * x) * x) * x
--
-- F はリストの台関手なので F(a,x) = 1 + a * x
-- g は上記の通り [0, (+)] になる.
--
--  h . [0, (+)] = [0, (+)] . F(f, h)
-- ==
--  h . [0, (+)] = [0, (+)] . (1 + f * h)
-- ==
--  h 0 = 0 /\ h . (+) = (+) . (f * h)
--
-- h 0 = 0
-- h (x + y) = f x + h y
--
-- h x = h (x + 0) = f x + h 0 = f x
-- ゆえに h = f しか取りえない.
--
-- h = f とすれば,
-- (|0, (+)|) . tri f = (|[0, (+)] . F(id, f)|) = (|[0, (+)] . (id + id * f)|) = (|0, (+) . (id * f) |)

honer f = foldr (0, \(a, b) -> a + f b)

-- | Ex 3.13
--
-- \sum_{i=0}^{n-1} i*a_i = 0*a0 + 1*a1 + 2*a2 + .. * i*ai + .. + (n-1)*a_{n-1}
--
-- ws = sum . listr mul . tri (succ * id) . listr <zero, id>
--
naiveWs = sum . map mul . tri (cross ((1+), id)) . map (pair (const 0, id))
pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
outl (x, y) = x
outr (x, y) = y
plus (x, y) = x + y

-- ここで tri (cross ((1+), id)) . map (pair (const 0, id)) が何をするか見ておく
--
-- tri (cross ((1+), id)) . map (pair (const 0, id)) $ [10,20,30,40,50]
-- => [(0,10),(1,20),(2,30),(3,40),(4,50)]
--
--  ws
-- == {- 仕様 -}
--  sum . listr mul . tri (succ * id) . listr <zero, id>
-- == {- sum = (|zero, plus|) -}
--  (|zero, plus|) . listr mul . tri (succ * id) . listr <zero, id>
-- == {- 型関手融合 (2.14) : (|h|) . Tg = (|h . F(g, id)|) -}
--  (|[zero, plus] . F(mul, id)|) . tri (succ * id) . listr <zero, id>
-- == {- F はリストの台関手 F(a,b) = 1 + a * b -}
--  (|zero, plus . (mul * id)|) . tri (succ * id) . listr <zero, id>
-- == {- ヒント: ペアの二番目は ws に与えられた引数の sum に当たる -}
--  outl . <(|zero, plus . (mul * id)|), (|zero, plus . (outr * id)|)> . tri (succ * id) . listr <zero, id>
-- == {- バナナスプリット則 : <(|h|),(|k|)> == (|<h . Foutl, k . Foutr>|) -}
--  outl . (|<[zero, plus . (mul * id)] . Foutl, [zero, plus . (outr * id)] . Foutr >|) . tri (succ * id) . listr <zero, id>
-- == {- F はリストの台関手 -}
--  outl . (|<[zero, plus . (mul * id)] . (id + id * outl), [zero, plus . (outr * id)] . (id + id * outr)>|)
--             . tri (succ * id) . listr <zero, id>
-- == {- 余積関手の融合則 [f,g] . (h + k) = [f . h, g . k] -}
--  outl . (|<[zero, plus . (mul * outl)], [zero, plus . (outr * outr)]>|) . tri (succ * id) . listr <zero, id>
-- == {- Ex 2.27 交換則 : <[f,g],[h,k]> == [<f,h>,<g,k>] -}
--  outl . (|<zero, zero>, <plus . (mul * outl), plus . (outr * outr)>|) . tri (succ * id) . listr <zero, id>
-- == {- h = <plus, outr> としてホーナー則を適用 : (|g|) . tri f = (|g . F(id, h)|) <= h . g = g . F(f, h) -}
--  outl . (|[<zero, zero>, <plus . (mul * outl), plus . (outr * outr)>] . F(id, <plus, outr>)|) . listr <zero, id>
-- == {- 型関手融合 -}
--  outl . (|[<zero, zero>, <plus . (mul * outl), plus . (outr * outr)>] . F(id, <plus, outr>) . F(<zero, id>, id)|)
-- == {- 関手則 -}
--  outl . (|[<zero, zero>, <plus . (mul * outl), plus . (outr * outr)>] . F(<zero, id>, <plus, outr>)|)
-- == {- F はリストの台関手 F(a,b) = 1 + a * b -}
--  outl . (|[<zero, zero>, <plus . (mul * outl), plus . (outr * outr)>] . (1 + <zero, id> * <plus, outr>)|)
-- == {- 余積関手の融合則 [f,g] . (h + k) = [f . h, g . k] -}
--  outl . (|<zero, zero>, <plus . (mul * outl), plus . (outr * outr)> . (<zero, id> * <plus, outr>)|)
-- == {- 後述 -}
--  outl . (|<zero, zero>, k|) where k (x, (y, z)) = (y+z, x+z)

-- 上記においてホーナー則の適用については <plus, outr> . g = g . F((succ * id), <plus, outr>) を示す必要がある.
-- f = (succ * id), g = [<zero, zero>, <plus . (mul * outl), plus . (outr * outr)>], h = <plus, outr>
--
--                            [(0,0),<plus . (mul * outl), plus . (outr * outr)>]
--     (0,0) + (a*b+c,b+d) <------------------------------------------------------- 1 + ((a, b), (c, d))
--                 |                                                                   |
--    <plus, outr> |                                                                   | F((succ * id), <plus, outr>)
--                 |                                                                   |  = id + (succ * id) * <plus, outr>
--                 v                                                                   v
-- (0,0) + (a*b+c+b+d, b+d) <------------------------------------------------------ 1 + ((a+1, b), (c+d, d))
--       =                    [(0,0),<plus . (mul * outl), plus . (outr * outr)>]
-- (0,0) + ((a+1)*b+c+d, b+d)
--       ~ (a*b+b+c+d, b+d)
--
-- 可換になる. よってホーナー則が適用できる.
--
-- 後述の箇所の論証
--
--  <plus . (mul * outl), plus . (outr * outr)> . (<zero, id> * <plus, outr>) $ (x, (y, z))
-- ==
--  <plus . (mul * outl), plus . (outr * outr)> $ (<zero, id> * <plus, outr>) (x, (y, z))
-- ==
--  <plus . (mul * outl), plus . (outr * outr)> $ (<zero, id> x, <plus, outr> (y, z))
-- ==
--  <plus . (mul * outl), plus . (outr * outr)> ((0, x), (y+z, z))
-- ==
--  ((plus . (mul * outl)) ((0, x), (y+z, z)), (plus . (outr * outr)) ((0, x), (y+z, z)))
-- ==
--  ((plus $ (mul * outl) ((0, x), (y+z, z)), plus $ (outr * outr) ((0, x), (y+z, z)))
-- ==
--  (plus $ (mul (0, x) , outl (y+z, z)), plus $ (outr (0, x), outr (y+z, z)))
-- ==
--  (plus (0 , y+z), plus $ (x, z))
-- ==
--  (y+z, x+z)
--
ws = outl . foldr ((0, 0), k)
  where k (x, (y, z)) = (y+z, x+z)

-- | Ex 3.14
--
data Treee a = Tip a
             | Nod (Treee a, Treee a)
             deriving Show

--           [tip,node]
--         Ta <---- a + Ta * Ta
--          |          |
-- u=(|f,g|)|          | id + u * u
--          v          v
--          X <---- a + X  * X
--             [f,g]

foldTreee (f, g) = u
  where u (Tip x) = f x
        u (Nod (l, r)) = g (u l, u r)

mapTreee f = foldTreee (Tip . f, Nod)

-- tri f = (|a . F(id, Tf)|)
--       = (|[tip, bin] . (id + Tf * Tf)|)
--       = (|tip, bin . (Tf * Tf)|)
triTreee f = foldTreee (Tip, Nod . (cross (mapTreee f, mapTreee f)))

max' :: Ord a => Treee a  -> a
max' = foldTreee (id, uncurry max)

zero = const 0
one = const 1

depths = triTreee succ . mapTreee zero

depth = max' . depths
depth' = foldTreee (zero, succ . uncurry max)

sumTreee = foldTreee (id, plus)

wpl = sumTreee . mapTreee mul . triTreee (cross (succ, id)) . mapTreee (pair (one, id))
--  wpl
-- == {- sumTree -}
--  (|id, plus|) . map mul . tri (succ * id) . map <one, id>
-- == {- 型関手融合 (2.14) : (|h|) . Tg = (|h . F(g, id)|) -}
--  (|[id, plus] . F(mul, id)|) . tri (succ * id) . map <one, id>
-- == {- F は木型の台関手 F(a, b) = a + b * b -}
--  (|[id, plus] . (mul + (id * id))|) . tri (succ * id) . map <one, id>
-- == {- 余積関手の融合則 [f,g] . (h + k) = [f . h, g . k] -}
--  (|mul, plus|) . tri (succ * id) . map <one, id>
-- == {- ヒント: ペアの二番目は wsl に与えられた引数の sum に当たる -}
--  outl . <(|mul, plus|), (|outr, plus|)> . tri (succ * id) . map <one, id>
-- == {- バナナスプリット則 : <(|h|),(|k|)> == (|<h . Foutl, k . Foutr>|) -}
--  outl . (|<[mul, plus] . Foutl, [outr, plus] . Foutr>|) . tri (succ * id) . map <one, id>
-- == {- F は木型の台関手 F(a, b) = a + b * b -}
--  outl . (|<[mul, plus] . (id + outl * outl), [outr, plus] . (id + outr * outr)>|) . tri (succ * id) . map <one, id>
-- == {- 余積関手の融合則 [f,g] . (h + k) = [f . h, g . k] -}
--  outl . (|<[mul, plus . (outl * outl)],[outr, plus . (outr * outr)]>|) . tri (succ * id) . map <one, id>
-- == {- Ex 2.27 交換則 : <[f,g],[h,k]> == [<f,h>,<g,k>] -}
--  outl . (|<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>|) . tri (succ * id) . map <one, id>
-- == {- h = <plus, outr> としてホーナー則を適用 : (|g|) . tri f = (|g . F(id, h)|) <= h . g = g . F(f, h) -}
--  outl . (|[<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>] . F(id, <plus, outr>)|) . map <one, id>
-- == {- 型関手融合 (2.14) : (|h|) . Tg = (|h . F(g, id)|) -}
--  outl . (|[<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>] . F(id, <plus, outr>) . F(<one, id>, id)|)
-- == {- 関手則 -}
--  outl . (|[<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>] . F(<one, id>, <plus, outr>)|)
-- == {- F は木型の台関手 F(a, b) = a + b * b -}
--  outl . (|[<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>] . (<one, id> + (<plus, outr> * <plus, outr>))|)
-- == {- 後述 -}
--  outl . (|f, g|) where f a = (a, a) /\ g ((a, b), (c, d)) = (a+b+c+d, b+d)
wpl' = foldTreee (f, g)
  where f a = (a, a)
        g ((a, b), (c, d)) = (a+b+c+d, b+d)

-- ホーナー則の適用についての論証は以下.
--  f = (succ * id), g = [<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>], h = <plus, outr> として
--  h . g = g . F(f, h) を示す必要がある.
--  <plus, outr> . g = g . F((succ * id), <plus, outr>) を示す.
--
--
--                   [<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>]
--     (a*b,b) + (x+z,y+w) <--------------------------------------------- (a, b) + ((x, y), (z, w))
--                 |                                                         |
--    <plus, outr> |                                                         | F((succ * id), <plus, outr>)
--                 |                                                         |  = (succ * id) + (<plus, outr> * <plus, outr>)
--                 v                                                         v
-- (a*b+b,b) + (x+z+y+w, y+w) <------------------------------------------ (a+1,b) + ((x+y, y), (z+w, w))
--       =           [<mul, outr>, <plus . (outl * outl), plus . (outr * outr)>]
-- ((a+1)*b,b) + ((x+y)+(z+w),y+w)
-- ~ (a+b+b,b) + (x+y+z+w,y+w)
--
-- 可換になる. よってホーナー則が適用できる.
--
--
-- 最後のステップはポイントワイズに計算すれば良い.
--
--  <mul, outr> . <one, id> a
-- ==
--  <mul, outr> $ (1, a)
-- ==
--  <mul, outr> (1, a)
-- ==
--  (a, a)
--
--  <plus . (outl * outl), plus . (outr * outr)> . (<plus, outr> * <plus, outr>) $ ((a,b), (c, d))
-- ==
--  <plus . (outl * outl), plus . (outr * outr)> $ (<plus, outr> * <plus, outr>) ((a,b), (c, d))
-- ==
--  <plus . (outl * outl), plus . (outr * outr)> $ ((a+b, b), (c+d, d))
-- ==
--  (a+b+c+d,b+d)
--
val :: [Float] -> Float
val = foldr (zero, shift)
  where zero = 0
        shift (d, r) = (d+r)/10

intern :: [Float] -> Int32
intern = round . val
round :: Float -> Int32
round r = floor $ (2^17 * r + 1)/2

round' = halve . convert
  where halve n = (n+1) `div`  2
        convert r = floor (2^17 * r)

-- | Ex 3.15
--
naiveVal =  sum . tri (/10) . map (/10)

--  val
-- == {- 定義 -}
--  sum . tri (/10) . listr (/10)
-- == {- sum = (|zero, plus|) -}
--  (|zero, plus|) . tri (/10) . listr (/10)
-- == {- h = (/10) でホーナー則 (|g|) . tri f = (|g . F(id, h)|) <= h . g = g . F(f, h) -}
--  (|[zero,plus] . F(id, (/10))|) . listr (/10)
-- == {-  -}
--  (|[zero,plus] . (id + id * (/10))|) . listr (/10)
-- == {-  -}
--  (|zero, plus . (id * (/10))|) . listr (/10)
-- == {- 型関手融合 (2.14) : (|h|) . Tg = (|h . F(g, id)|) -}
--  (|[zero, plus . (id * (/10))] . F((/10), id)|)
-- == {-  -}
--  (|[zero, plus . (id * (/10))] . (id + ((/10) * id))|)
-- == {-  -}
--  (|zero, plus . (id * (/10)) . ((/10) * id)|)
-- == {-  -}
--  (|zero, plus . ((/10) * (/10))|)
-- == {- 後述 -}
--  (|zero, (/10) . plus|)

val' = foldr (0, (/10) . plus)

-- ホーナー則の適用については g = [zero, plus], f = (/10), h = (/10) として h . g = g . F(f, h) を示せばよい.
-- つまり (/10) . [zero, plus] = [zero, plus] . F((/10), (/10)) を示せばよい.
--
--                [zero, plus]
--       0+(a+b)  <----------- * + (a,b)
--              |              |
--        (/10) |              | F((/10), (/10))
--              v              v
-- 0/10+(a+b)/10  <----------- * + (a/10,b/10)
--                [zero, plus]
--
-- 可換であることが確認できるのでホーナー則を適用できる.
--
intern' = halve . foldr (0, cshift)
  where halve n = (n+1) `div`  2
        cshift (d, n) = (2^17 * d + n) `div` 10

-- | Ex 3.16
--
intern2 = halve . foldr (0, cshift)
  where halve n = (n+1) `div` 2
        cshift (d, n) = (2^3 * d + n) `div` 10
-- ???

-- | Ex 3.17
--
-- ???

-- | Ex 3.18
--
-- 間接的同値則
--  forall k. k <= m == k <= n  <=> m == n
--
--  任意の k について等しいなら, k := m と取った時 m <= m == m <= n で m <= n が真であり,かつ
--  k := n と取った時 n <= m == n <= n で n <= m も真である. よって反対称律から m == n なので => の向きの imply は証明できる.
-- 逆は自明.
--
-- 上の証明は整数であることや整数上の順序であることを使っていない.
-- 反対称律を使っているので半順序集合であることだけが要求される.
-- よって任意の poset (半順序集合) において成り立つ.
-- 
-- | Ex 3.19
--
--  切り下げの普遍性
--   n <= x == n <= floor x
--  を使って切り下げ則
--   floor ((a+r)/b) == floor ((a+foor(r))/b)
--  を証明する.
--
--  n <= floor ((a+r)/b)
-- == {- floor の普遍性 -}
--  n <= (a+r)/b
-- == {- b > 0 と計算から -}
--  n * b - a <= r
-- == {- floor の普遍性 (n * b - a は整数) -}
--  n * b - a <= floor (r)
-- == {- 計算 -}
--  n <= (a + floor (r))/b
-- == {- floor の普遍性 -}
--  n <= floor ((a + floor (r))/b)
--

-- | Ex 3.20
--
--  切り下げ則
--   floor ((a+r)/b) == floor ((a + floor(r))/b)
-- a = 0 b = 2/3 r = 3/2 とすると左辺は
--   floor ((0+3/2)/(2/3))
-- ==
--   floor ((3/2)/(2/3))
-- ==
--   floor (9/4)
-- ==
--   2
--
-- 右辺は
--   floor ((0 + floor(3/2))/2/3)
-- ==
--   floor ((1/(2/3)))
-- ==
--   floor (3/2)
-- ==
--   1

-- | Ex 3.21
--
-- f : A <- B が単射なら任意の二項演算 (+) : B <- C x B に対して
--   f (c (+) b) = c (*) f b
-- なる二項演算 (*) : A <- A x B の存在を示す.
--
-- f が単射なら f^-1 が存在して, f^-1 . f = id.
-- (*) を
-- c (*) d = f (c (+) (f^-1 d))
-- と定義する.
-- すると,
-- c (*) f b = f (c (+) (f^-1 (f b)))
--           = f (c (+) id b)
--           = f (c (+) b)

-- | Ex 3.22
--
--  f : A <- B かつ (+) : B <- C x B として
-- f b0 = f b1 かつ f (c (+) b0) /= f (c (+) b1) ならば
-- f (c (+) b) = c (*) f b となるような (*) は存在しない.
--
-- この定理を使って round . val に融合則を適用できないことを示す.
--
-- intern = round . val = round . foldr (0, shift)
--                            where shift (d, r) = (d+r)/10
-- なので,ここで融合則
--  h . (|f|) = (|g|) <= h . f = g . Fh
-- を適用しようとすると, round . [0, shift] = g . Fround なる g が存在することを示す必要があるが,
-- これが上記の定理を使って使えないことを示せばよい.
-- g = [c, (*)] とした場合, round [0, shift] = [c, (*)] . (id + id * round) = [c, (*) . (id * round)]
-- よって, round 0 = 0 = c なので c = 0 として, round . shift = (*) . (id * round) が融合則の前件になる.
-- ポイントワイズにすると, round (shift (d, n)) = ((*) . (id * round)) (d, n) = (*) (d, round n) = d (*) round n
-- 一方shift = (+) とおくと, round (d (+) n) = d (*) round n となる.
-- これを定理に当てはめるとすると,
--   round (d (+) n) = d (*) round n
--     f   (c (+) b) = c (*)   f   b
-- なので定理から, round b0 = round b1 かつ round (c `shift` b0) /= round (c `shift` b1) を示せれば融合則の前件を却下できる.
ex_3_22 = let (c, b0, b1) = (0, 0.100001, 0.100000)
              d `shift` r = (d+r)/10
              round r = floor $ (2^17 * r + 1)/2
          in round b0 == round b1 && round (c `shift` b0) /= round (c `shift` b1)

-- ???
-- 原書の回答はどうも Int32 と Float なら微妙に成り立ちそう.
-- 2147483647 は 2^31-1 のことで maxBound :: Int32 である. だが現在のGHCだとアンダーフローして同じにはならない.
-- 型をInt32とFloatで固めると round 10000.1 = 655366528 と round 10000.0 = 655360000 となり公開されている回答とも整合しそう.

-- | Ex 3.23
--
-- outl : A <- A x 0
-- outr : 0 <- A x 0
--    i : A <- 0
--
-- outl = i . outr
-- unnull . null = id /\ null . unnull = id となる unnull が存在しなければならないことを示す.
--
-- unnull = <i, id>, null = outr ととれば任意の圏で成り立つ.
--
--  null . unnull
-- ==
--  outr . <i, id>
-- ==
--  id
--
-- 逆向きも論証する.
--
--  unnull . null
-- ==
--  <i, id> . outr
-- == {- 対構成の融合 -}
--  <i . outr, outr>
-- == {- outl = i . outr なので -}
--  <outl, outr>
-- == {- 反射則 -}
--  id

-- | Ex 3.24
--
-- Rel は分配的ではない.
undistr :: Either (a, b) (a, c) -> (a, Either b c)
undistr = either (cross (id, inl)) (cross (id, inr))
  where inl = Left
        inr = Right

distr :: (a, Either b c) -> Either (a, b) (a, c)
distr (a, Left  b) = Left  (a, b)
distr (a, Right c) = Right (a, c)

-- Rel における積の定義は余積の定義と一致する.
-- すなわち A * B = A + B と定義できる.
-- 依って分配則
--
--   A * (B + C) == (A * B) + (A * C)
--
-- は
--
--   A + (B + C) == (A + B) + (A + C)
--
-- となるがこれは一致しない.
-- (もし分からなければ and と quad の議論などを振り返れ)
--

-- | Ex 3.25
--
-- ? の定義
--
-- p? :: A + A <- A
-- p? = (unit+unit).distr.<id,p>
--
--   ただし p :: Bool <- A
--
-- ヒント: (! + !) . distr = (! + !) . outr を証明する.
--  (! + !) . distr
-- == {- (2.3) !の融合則 !a . f = !b (f:a <- b) -}
--  (!.outr + !.outr) . distr
-- == {- 余積関手 -}
--  (! + !) . [inl . outr, inr . outr] . distr
-- == {- outr は outr <- (x) な自然変換 -}
--  (! + !) . [outr . (id x inl), outr . (id x inr)] . distr
-- == {- 余積関手の融合則 -}
--  (! + !) . outr . [(id x inl), (id x inr)] . distr
-- == {- undistr = [(id x inl), (id x inr)] -}
--  (! + !) . outr . undistr . distr
-- == {- undistr . distr = id -}
--  (! + !) . outr
--  
--  (p?)??
-- == {- p? の定義 -}
--  ((unit + unit) . distr . <id, p>)??
-- == {- ??の定義 t?? = (!+!).t -}
--  (! + !) . ((unit + unit) . distr . <id, p>)
-- == {- unit = outl (unit : A <- A x 1) -}
--  (! + !) . (outl + outl) . distr . <id, p>
-- == {- 余積関手の合成 (f + g) . (h + k) = f . h + g . k -}
--  (! . outl + ! . outl) . distr . <id, p>
-- == {- (2.3) !の融合則 !a . f = !b (f:a <- b) -}
--  (! + !) . distr . <id, p>
-- == {- ヒント -}
--  (! + !) . outr . <id, p>
-- == {- 対構成の消去則 (2.5) -}
--  (! + !) . p
-- == {- p : 1+1 <- A で !の反射則(2.2) !_1 = id_1 から -}
--  p
--
-- 最後のところ, ! : 1 <- A であることに注意して, p : 1 + 1 <- A なので,
-- p で 1 + 1 に移したものをそれぞれ単に id_1 : 1 <- 1 で写しただけなので
-- (! + !) を単に消去してもよくなることから求まる.

-- | Ex 3.26
--
-- (unit + unit) . distr が F <- G な自然変換(多相関数)であることを示す.
-- F(A) = A + A
-- G(A) = A * Bool
-- distr : A*(B+C) <- A*B + A*D
--
--  以下図式が可換になることから証明できる.
--
--              unit+unit           distr
--  F(A) = A + A <---- A*1 + A*1 <----- A * Bool = G(A)
--           |                            |
--  F(f)=f+f |                            | f*id=G(f)
--           |                            |
--           v                            v
--  F(B) = B + B <---- B*1 + B*1 <----- B * Bool = G(B)
--               unit+unit          distr
--
-- | Ex 3.27
--
-- 分配的な圏で h: 0 <- A がある.
-- h が同型射であることと,それゆえ A が始対象でもあることを示す.
-- i : A <- 0 なので h . i = id_0 は成り立つ.
--  i . h
-- == {- 対構成の消去則 -}
--  i . outr . <id, h>
-- == {- Ex 3.23 から i . outr = outl -}
--  outl . <id, h>
-- == {- 対構成の消去則 -}
--  id
--
--  ゆえに h は同型射. よって A ~= 0 なので A は始対象でもある.
--
-- | Ex 3.28
--
-- f * [g, h]  = [f * g, f * h] . distr を示す
--
--  [f * g, f * h] . distr
-- == {- 余積の消去則 -}
--  [f * ([g, h] . inl), f * ([g, h] . inr)] . distr
-- == {- id 単位元 -}
--  [(f . id) * ([g, h] . inl), (f, id) * ([g, h] . inr)] . distr
-- == {- 積関手 (f * g) . (h * k) = (f . h) * (g . k) -}
--  [(f * [g, h]) . (id * inl), (f * [g, h]) . (id * inr)] . distr
-- == {- 余積の融合則 m . [h, k] = [m . h, m . k] -}
--  (f * [g, h]) . [(id * inl), (id * inr)] . distr
-- == {- [id * inl, id * inr] = undistr で undistr . distr = id -}
--  f * [g, h]
--
-- | Ex 3.29
--
-- Bool = 1 + 1
-- よって
-- Bool^2 = Bool * Bool = (1+1) * (1 + 1)
--  (1 + 1) * (1 + 1)
-- == {- 分配則 A * (B + C) == (A * B) + (A * C) -}
--  ((1 + 1) * 1) + ((1 + 1) * 1)
-- == {- Ex 2.26 A * B == B * A -}
--  (1 * (1 + 1)) + (1 * (1 + 1))
-- == {- 分配則 A * (B + C) == (A * B) + (A * C) -}
--  (1 * 1 + 1 * 1) + (1 * 1 + 1 * 1)
-- == {- Ex 2.26 A * 1 == A -}
--  (1 + 1) + (1 + 1)
--
-- | Ex 3.30
--
-- filter p . listr f = listr f . filter (p . f) を示す.
-- ただし filter p = concat . listr (p -> wrap, nil) とする.
--
--  filter p . listr f
-- == {- filter の定義 -}
--  concat . listr (p -> wrap, nil) . listr f
-- == {- listr 関手 -}
--  concat . listr ((p -> wrap, nil) . f)
-- == {- (3.4) -}
--  concat . listr ((p . f) -> (wrap . f), nil)
-- == {- wrap の自然性 -}
--  concat . listr ((p . f) -> (listr f . wrap), nil)
-- == {- nil の自然性 -}
--  concat . listr ((p . f) -> (listr f . wrap), listr f . nil)
-- == {- (3.3) -}
--  concat . listr ((listr f) . ((p . f) -> wrap, nil))
-- == {- 関手則 -}
--  concat . listr (listr f) . listr ((p . f) -> wrap, nil)
-- == {- concat の自然性 : listr f . concat = concat . listr (listr f) -}
--  listr f . concat . listr ((p . f) -> wrap, nil)
-- == {- $\mathit{filter}$ の定義 -}
--  listr f . filter (p . f)
--
-- | Ex 3.31
--
-- cat . (nil * id) = outr
-- cat . (cons * id) = cons . (id * cat) . assocr
--
-- が等式 (3.6)
--  cat . ([nil, cons] * id) = [outr, cons] . (id + id * cat) . phi
--    ただし phi = (id + assocr) . distl
--
-- と同等であることを示す.
-- distl : (A * C) + (B * C) <- (A + B) * C
-- assocr : A * (B * C) <- (A * B) * C
--
-- 等式 (3.6) を基点に進める.
-- 左辺側から
--  cat . ([nil, cons] * id)
-- == {- Ex 3.28 に双対(distrのケース)が表れている -}
--  cat . [nil * id, cons * id] . distl
-- == {- 余積の融合則 m . [h, k] = [m . h, m . k] -}
--  [cat . (nil * id), cat . (cons * id)] . distl
--
-- 念のため, Ex 3.28 の双対を示しておく
--
--  [f * h, g * h] . distl
-- == {- 余積の消去則 -}
--  [([f, g] . inl) * h, ([f, g] . inr) * h] . distl
-- == {- id 単位元 -}
--  [([f, g] . inl) * (h . id), ([f, g] . inr) * (h . id)] . distl
-- == {- 積関手 (f * g) . (h * k) = f . h * g . k -}
--  [([f, g] * h) . (inl * id), ([f, g] * h) . (inr * id)] . distl
-- == {- 余積の融合則 m . [h, k] = [m . h, m . k] -}
--  ([f, g] * h) . [(inl * id), (inr * id)] . distl
-- == {- [inl * id, inr * id] = undistl : (A*C)+(B*C)<-(A+B)*C で undistl . distl = id -}
--  [f, g] * h
--
--
-- 右辺側から
--  [outr, cons] . (id + id * cat) . phi
-- == {- 余積の吸収則 [f, g] . (h + k) = [f . h, g . k] -}
--  [outr, cons . (id * cat)] . phi
-- == {- phi の定義 -}
--  [outr, cons . (id * cat)] . (id + assocr) . distl
-- == {- 余積の吸収則 [f, g] . (h + k) = [f . h, g . k] -}
--  [outr, cons . (id * cat) . assocr] . distl
--
-- distl が同型射であることと余積の普遍性から同等である.


-- | Ex 3.32
--
-- 余積を持つ任意のデカルト閉圏つまり積, 余積, 指数がある任意の圏は分配的であることを示す.
-- 分配的な圏の要件は2つだった.
--
--  1. distl : (A * C) + (B * C) <- (A + B) * C が存在して同型射である.(undistlが逆射)
--  2. null : 0 <- A * 0 が存在して同型射である.(unnull が逆射)
--
-- i : 0^A <- 0 (= 0 <- A <- 0)
-- null : 0 <- A * 0
-- distl : (A * C) + (B * C) <- (A + B) * C
--
-- inl : (A * C) + B <- (A * C)
-- curry inl : (A * C) + B <- C <- A
-- inr : A + (B * C) <- (B * C)
-- curry inr : A + (B * C) <- C <- B
--
-- 以下の通り null distl が実装できる.
--
data Void = Void
i a Void = Void
null' = uncurry i
distl = uncurry (either (curry Left) (curry Right))

-- unnull は実装できるか?
-- undistl も実装できるか?

-- | Ex 3.33
--
-- * A^0 ~= 1
--   - 1 <- A^0 は ! で実装可能
--   - A^0 <- 1 は以下で合成する.
--
--         i     unit      swap
--     A <--- 0 <--- 0 * 1 <--- 1 * 0
--   これで i . unit . swap : A^0 <- 1 * 0 なので
--   curry (i . unit . swap) : A <- 0 <- 1 でありしたがって A^0 <- 1 である
--
test_3_33_1l :: Exp a Void -> ()
test_3_33_1l = ban
  where ban = const ()
test_3_33_1r :: () -> Exp a Void
test_3_33_1r = curry (i . unit . swap)
  where swap (a, b) = (b, a)
        unit (x, ()) = x
        i = undefined
--
-- * A^1 ~= A
--   - A <- A^1 は以下を合成する.
--
--   なお apply : A <- A^B * B
--        ! : 1 <- A
--   なので, 
--
--      apply         <id, !>
--    A <--- A^1 * 1 <------ A^1
--
--    apply . <id, !> : A <- A^1
--
--   - A^1 <- A
--     unit : A <- A * 1 なので,
--     curry unit : A <- 1 <- A つまり A^1 <- A である.
test_3_33_2l :: Exp a () -> a
test_3_33_2l = apply . pair (id, ban)
  where apply = uncurry ($)
        ban = const ()
test_3_33_2r :: a -> Exp a ()
test_3_33_2r = curry unit
  where unit (x, ()) = x
--
-- * A^(B+C) ~= A^B * A^C
--   - A^B * A^C <- A^(B+C) は以下で合成する.
--     まず,
--
--         apply                     id * inl
--   A <---------- A^(B+C) * (B+C)  <-------- A^(B+C) * B
-- 
--         apply                     id * inr
--   A <---------- A^(B+C) * (B+C)  <-------- A^(B+C) * C
--
--   これらをそれぞれカリー化すると,
--   curry (apply . (id * inl)) : A <- B <- A^(B+C) == A^B <- A^(B+C)
--   curry (apply . (id * inr)) : A <- C <- A^(B+C) == A^C <- A^(B+C)
--   よって
--   pair (curry (apply . (id * inl)), curry (apply . (id * inr))) : A^B * A^C <- A^(B+C)
--
type Exp a b = b -> a
type a :+: b = Either a b
type a :*: b = (a, b)
test_3_33_3l :: Exp a (b :+: c) -> (Exp a b) :*: (Exp a c)
test_3_33_3l = pair (l, r)
  where l = curry (apply . (cross (id, Left)))
        r = curry (apply . (cross (id, Right)))
        apply = uncurry ($)
--
--   - A^(B+C) <- A^B * A^C は以下で合成する.
--     まず,
--
--       apply           outl * id
--   A <------- A^B * B <---------- (A^B * A^C) * B
--
--       apply           outr * id
--   A <------- A^C * C <---------- (A^B * A^C) * C
--
--        [l, r]                                           distr
--   A <---------- ((A^B * A^C) * B) + ((A^B * A^C) * C) <------- (A^B * A^C) * (B + C)
--   つまり [l, r] . distr : A <- (A^B * A^C) * (B + C)
--   よって curry ([l, r] . distr) : A <- (B + C) <- (A^B * A^C) == A^(B+C) <- (A^B * A^C)
--
test_3_33_3r :: (Exp a b) :*: (Exp a c) -> Exp a (b :+: c)
test_3_33_3r = curry (either l r . distr)
  where l = apply . cross (outl, id)
        r = apply . cross (outr, id)
        apply = uncurry ($)

-- | Ex 3.34
--
-- f : A <- B と g : A^B <- 1 の間の全単射を構成する.
-- f を curry (f . unit) に移せばよい.
--
--       f     unit       swap
--   A <--- B <--- B * 1 <--- 1 * B
--
-- これをカリー化すると curry (f . unit . swap) : A <- B <- 1
test_3_34 :: (b -> a) -> (() -> Exp a b)
test_3_34 f = curry (f . unit . swap)
  where unit (x, ()) = x
        swap (x, y) = (y, x)
--
--      A^B <--- 1
--         ||
-- (A <--- B <--- 1)
test_3_34inv :: (() -> Exp a b) -> (b -> a)
test_3_34inv g = g ()

-- | Ex 3.35
--
-- 前順序(A, <=)は練習問題2.6より
-- 対象をAの要素として a <= b のときに a <- b がただ1本だけ存在する圏である.
--
-- デカルト閉は積があることと対象aとbに対して指数a^bが存在すること.
-- 指数の定義
--  2つの対象 A と B の指数とは,対象 A^B と射 apply : A <- A^B * B のことであり,
--  各 f : A <- C * B に対して
--    apply . (curry f * id) = f
--  となるような一意な射 curry f : A^B <- C が存在するようなもの.
--
-- 指数の普遍性による定義
--   g = curry f == apply . (g * id) = f
--
--   L g = apply . (g * id) = uncurry ($) . (cross (g, id)) = uncurry g が左随伴
--   R f = curry f が右随伴
--
-- ???

-- | Ex 3.36
--
-- f^B = curry (f . apply)
--
-- f : A <- C
-- apply : C <- C^B * B
--
--     f     apply
-- A <--- C <--- C^B * B
--
-- すると curry (f . apply) : A <- B <- C^B == (A <- B) <- (C <- B) == A^B <- C^B == F(A) <- F(C)
-- ただし F(X) = X^B とする
-- つまり curry (f . apply) が共変関手であることが分かる.
--

-- | Ex 3.37
--
-- Aがデカルト閉ならA^Bもデカルト閉であることを示す.
-- A^Bは Ex 2.19 によると
--  対象: 関手 A <- B
--  射: 自然変換
--
-- A^Bは圏Bから圏Aへの関手を対象とする圏で,そのコドメインAがデカルト閉である.
-- つまりコドメインがデカルト閉であるような関手圏はデカルト閉であることを示す.
--
--                B
--          --------------
--
--                f
--            x ----> y
--
--
--          /    |     \
--         /     |      \
--      H /      |G      \ F
--       /       |        \
--      v        v         v
--
--                A
--          --------------
--
--               phi
--     Gx   -------------> Fx
--     |                    |
--   Gf|                    |Ff
--     v                    v
--     Gy   -------------> Fy
--               phi
--
-- さて,この圏Aはデカルト閉なので任意の有限積と指数がある.
-- つまり任意の対象FとGについて指数F^Gが存在する.
-- すなわち指数は (F^G)(x) = Fx^Gx である.
--
-- | Ex 3.38
--
-- map : (b -> a) -> [b] -> [a] == a^b -> [a]^[b]
--
-- G(a,b) : [a]^[b]
-- F(a,b) : a^b
-- map : G <- F
--
--                          map                                     f
--      [a]^[b] = G(a,b) <------- F(a, b) = a^b                 a <--- b
--                  |                |                          |      ^
--            G(h,k)|                |F(h,k)                    |h     |k
--                  v                v                          v      |
--      [c]^[d] = G(c,d) <------- F(c, d) = c^d                 c <--- d
--                          map                                 h . f . k
--
--  F,G : Fun <- Fun * Fun^op
--
--  G(h,k) . map = map . F(h, k)
-- つまり map は G <- F という関手の間の自然変換である.
