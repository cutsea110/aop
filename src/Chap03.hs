{-# LANGUAGE NPlusKPatterns #-}
module Chap03 where

import Prelude hiding (foldr, sum, product, length, div)

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

average = div . pair (sum, length)
  where
    pair (f, g) x = (f x, g x)
    div (x, y) = x / y

avg = div . foldr (zeross, pluss)
      where zeross = (0, 0)
            pluss (a, (b, n)) = (a + b, n + 1)
            div (x, y) = x / y
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

-- 上記においてホーナー則の適用については <plus, outr> . g = g . F((succ * id), <plus, outr>) を示す必要がある.-
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
