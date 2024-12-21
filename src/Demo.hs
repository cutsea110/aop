{-# LANGUAGE LambdaCase, RankNTypes #-}
module Demo where
-- --------------------------------------
-- Demo
-- --------------------------------------

--
-- 相互再帰的なデータ型の cata/ana の図式の描き方
--

data Tree a = Fork a (Forest a) deriving (Show, Eq)
data Forest a = Null | Grows (Tree a) (Forest a) deriving (Show, Eq)

--                     Fork
--    Ta * Fa <==================== (a * Fa) * (1 + Ta * Fa)
--    |    |        [Null, Grows]      |          |
--    |    |                           |          |
--   u|   v|                           |id*v      |id+u*v
--    |    |                           |          |
--    |    |                           |          |
--    v    v            h              v          v
--    X  * Y  <==================== (a * Y ) * (1 + X  * Y )
--                    [c, g]
--
foldt phi@(h, c, g) (Fork a fs) = h (a, foldf phi fs)
foldf phi@(h, c, g) Null = c
foldf phi@(h, c, g) (Grows t fs) = g (foldt phi t, foldf phi fs)

--                     Fork
--    Ta * Fa <==================== (a * Fa) * (1 + Ta * Fa)
--    A    A        [Null, Grows]      A          A
--    |    |                           |          |
--   u|   v|                           |id*v      |id+u*v
--    |    |                           |          |
--    |    |                           |          |
--    |    |            f              |          |
--    X  * Y  ====================> (a * Y ) * (1 + X  * Y )
--                      g
--
unfoldt psi@(f, g) x = case f x of
  (a, y) -> Fork a (unfoldf psi y)
unfoldf psi@(f, g) y = case g y of
  Nothing -> Null
  Just (x, y') -> Grows (unfoldt psi x) (unfoldf psi y')

(genT, genF) = (unfoldt (f, g), unfoldf (f, g))
  where
    f x = (x, x-1)
    g y = if y <= 0 then Nothing else Just (y, y-1)

drawT :: Show a => Tree a -> String
drawT = concat . foldt (h, c, g)
  where
    h (a, fs) = ["+-- " ++ show a ++ "\n"] ++ map ("  " `joint`) fs
      where cs `joint` ds@('+':_) = cs ++ ds
            cs `joint` ds         = cs ++ '|':ds
    c = []
    g (t, fs) = t ++ fs

drawF :: Show a => Forest a -> String
drawF = concat . foldf (h, c, g)
  where
    h (a, fs) = ["+-- " ++ show a ++ "\n"] ++ map ("  " `joint`) fs
      where cs `joint` ds@('+':_) = cs ++ ds
            cs `joint` ds         = cs ++ '|':ds
    c = []
    g (t, fs) = t ++ fs

-- mapt, mapf
-- f : a -> b
--
--                                Fork
--    Ta * Fa <============================================= (a * Fa) * (1 + Ta * Fa)
--    |    |                 [Null, Grows]                      |          |
--    |    |                                                    |          |
--   u|   v|                                                    |id*v      |id+u*v
--    |    |                                                    |          |
--    |    |                                                    |          |
--    v    v    Fork                                f*id        v          v
--    Tb * Fb <======= (b * Fb) * (1 + Tb * Fb) <=========== (a * Fb) * (1 + Tb * Fb)
--           [Null, Grows]                         id+id*id
--
mapt f = foldt (h, c, g)
  where
    h (a, fs) = Fork (f a) fs
    c = Null
    g (t, fs) = Grows t fs
mapf f = foldf (h, c, g)
  where
    h (a, fs) = Fork (f a) fs
    c = Null
    g (t, fs) = Grows t fs


(lenT, lenF) = (foldt (h, c, g), foldf (h, c, g))
  where
    h (a, fs) = 1 + fs
    c = 0
    g (t, fs) = t + fs

(sumT, sumF) = (foldt (h, c, g), foldf (h, c, g))
  where
    h (a, fs) = a + fs
    c = 0
    g (t, fs) = t + fs

-- zipt, zipf
--
--                     Fork
--    Ta * Fa <==================== (a * Fa) * (1 + Ta * Fa)
--    |    |        [Null, Grows]      |          |
--    |    |                           |          |
--   u|   v|                           |id*v      |id+u*v
--    |    |                           |          |
--    |    |                           |          |
--    |    |                           |          |
--    v Tb v  Fb         h             v      Fb  v        Tb     Fb
--   Taxb * Faxb  <================== (a * Faxb) * (1 + Taxb * Faxb)
--                    [c, g]
(zipt, zipf) = (foldt (h, c, g), foldf (h, c, g))
  where
    h (a, fs) = \(Fork b fb) -> Fork (a, b) (fs fb)
    c = \Null -> Null
    g (t, fs) = \(Grows tb fb) -> Grows (t tb) (fs fb)



----
