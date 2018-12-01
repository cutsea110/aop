{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, RankNTypes, TupleSections #-}
-- Ref.) https://stackoverflow.com/questions/13352205/what-are-free-monads/13352580
module Free where


data Free f a = Pure a
              | Roll (f (Free f a))

pure' = Pure
roll = Roll

--    In = [pure, roll]
-- Ta <-------------- a + F(Ta)
-- |                    |
-- | u                  | 1 + Fu
-- |                    |
-- v                    v
-- X  <-------------- a + FX
--       [f, g]

cata (f, g) (Pure x) = f x
cata (f, g) (Roll x) = g (fmap (cata (f, g)) x) -- this fmap is over F not T (= Free f)

instance Functor f => Functor (Free f) where
    fmap f = cata (pure' . f, roll)
--     fmap f (Pure x) = pure' (f x)
--     fmap f (Roll x) = roll (fmap (fmap f) x) -- 1st fmap is over F and 2nd fmap is the same as lhs (= Free f)

mu :: Functor f => Free f (Free f a) -> Free f a
mu = cata (id, roll)
-- mu (Pure x) = x
-- mu (Roll x) = roll (fmap mu x)

-- Ref.) https://stackoverflow.com/questions/22121960/applicative-instance-for-free-monad
instance Functor f => Applicative (Free f) where
    pure = pure'
    f <*> x = cata ((<$> x), roll) f -- cata ((`fmap` x), roll) f
--    (<*> x) (Pure f) = (<$> x) f
--    (<*> x) (Roll f) = roll (fmap (<*> x) f)

instance (Functor f, Applicative (Free f)) => Monad (Free f) where
    return = pure
    x >>= f = f =<< x where (=<<) = (mu.).fmap

-- example.
-- L(A) = F(GA, HA) where G,H are unary functor.
-- phi :: H <- L imples H <- TG where (alpha, T) is initial type over F(AOP exercise 2.39)
-- 
-- And then, consider the case when substituted G to Identity functor,and substitute H to `B'.
-- So, the initial F-Algebra of TA <- F(A, G(TA))) means Binary Tree(Leafy). 
data B a = B a a deriving Show

instance Functor B where
    fmap f (B x y) = B (f x) (f y)

type Tree a = Free B a
tip :: a -> Tree a
tip x = pure x
bin :: Tree a -> Tree a -> Tree a
bin l r = Roll (B l r)

instance Show a => Show (Tree a) where
    show (Pure a) = "Pure " ++ show a
    show (Roll (B x y)) = "Roll (B {" ++ show x ++ "," ++ show y ++ "})" 

size :: Tree a -> Int
size = cata (const 1, plus)
    where
        plus (B x y) = x + y

depth :: Tree a -> Int
depth = cata (const 0, (+1).maxB)
    where
        maxB (B x y) = max x y
-- depth (Pure a) = 0
-- depth (Roll (B x y)) = max (depth x) (depth y) + 1

withDepth :: Tree a -> Tree (a, Int)
withDepth = sub 0
    where
        sub d (Pure x) = tip (x, d)
        sub d (Roll (B x y)) = bin (sub (d+1) x) (sub (d+1) y)

data LR = L | R deriving (Show, Eq)
lr (l, r) L = l
lr (l, r) R = r

assocr :: ((a, b), c) -> (a, (b, c))
assocr ((x, y), z) = (x, (y, z))

withRoute :: Tree a -> Tree (a, [LR])
withRoute = sub []
    where
        sub lrs (Pure x) = tip (x, lrs)
        sub lrs (Roll (B x y)) = bin (sub (L:lrs) x) (sub (R:lrs) y)

drawTree :: (Show a) => Tree a -> String
drawTree t = "+" ++ (draw . fmap assocr . withRoute . withDepth) t
    where
        draw (Pure (x, (d, []))) = " " ++ show x ++ "\n"
        draw (Pure (x, (d, lrs@(L:_)))) | all (==L) lrs = concat (replicate d "--+") ++ show x ++ "\n"
                                        | otherwise     = "|" ++ concat (replicate (d-1) "  |") ++ "\n+" ++ concat (replicate d "--+") ++ show x ++ "\n"
        draw (Pure (x, (d, lrs@(R:_)))) | all (==R) lrs = concat (replicate (d-1) "   ") ++ "|\n " ++ concat (replicate (d-1) "  +") ++ "--+" ++ show x ++ "\n"
                                        | otherwise     = "|" ++ concat (replicate (d-1) "  |") ++ "\n+" ++ concat (replicate (d-1) "  +") ++ "--+" ++ show x ++ "\n"
        draw (Roll (B l r)) = draw l ++ draw r

test = do
    x <- tip 1
    y <- tip 'a'
    return (x, y)
test1 = do
    x <- bin (tip 1) (tip 2)
    y <- tip 3
    return (x, y)
test2 = do
    x <- tip 1
    y <- bin (tip 2) (tip 3)
    return (x, y)
test3 = do
    x <- bin (tip 1) (tip 2)
    y <- bin (tip 'a') (tip 'b')
    return (x, y)
test4 = do
    x <- bin (tip 1) (tip 2)
    y <- bin (tip 'a') (bin (tip 'b') (tip 'c'))
    return (x, y)
test5 = do
    x <- bin (tip 1) (tip 2)
    y <- bin (tip 'a') (bin (tip 'b') (tip 'c'))
    return (y, x)
test6 = do
    x <- bin (tip 'a') (bin (tip 'b') (tip 'c'))
    y <- bin (tip 1) (tip 2)
    return (x, y)
test7 = do
    x <- bin (bin (tip 1) (tip 2)) (tip 3)
    y <- bin (tip 'a') (bin (tip 'b') (tip 'c'))
    return (x, y)
test8 = bin (tip 1) (bin (tip 2) (bin (tip 3) (bin (tip 4) (bin (tip 4) (tip 6)))))

-- >>> let tr = bin (tip 2) (tip 1)
-- >>> let tl = bin (tip 4) (tip 3)
-- >>> let t = bin (tip 5) (bin tl tr)
-- >>> let f1 = tip (*2)
-- >>> fmap (*2) t
-- >>> f1 <*> t
-- >>> let f2 = bin (tip (*2)) (tip (+2))
-- >>> f2 <*> tl
-- >>> f2 <*> t
monadTest = do
    x <- bin (tip 'a') (bin (tip 'b') (tip 'c'))
    y <- bin (bin (tip 1) (tip 2)) (tip 3)
    return (x,y)

monadTest2 = do
    f <- bin (tip (+2)) (bin (tip (*2)) (tip (^2)))
    x <- bin (bin (tip 1) (tip 2)) (tip 3)
    return (f x)

monadTest3 = do
    f <- tip $ bin (tip (+2)) (bin (tip (*2)) (tip (^2)))
    x <- tip $ bin (bin (tip 1) (tip 2)) (tip 3)
    f <*> x

{-
-- the case of f is Identity
-- F(A, TA) = A + TA
data Free a = Pure a
            | Roll (Free a)
            deriving (Show, Eq)

pure' = Pure
roll = Roll

--    In = [pure, roll]
-- Ta <-------------- a + Ta
-- |                    |
-- | u                  | 1 + u
-- |                    |
-- v                    v
-- X  <-------------- a + X
--       [f, g]

cata :: (a -> b, b -> b) -> Free a -> b
cata (f, g) (Pure x) = f x
cata (f, g) (Roll x) = g (cata (f, g) x)

-- T f = cata (alpha . F(f, id))
--     = cata ([pure, roll] . (f + id))
--     = cata (pure . f, roll)
freeMap :: (a -> b) -> Free a -> Free b
freeMap f = cata (pure' . f, roll)

instance Functor Free where
    fmap = freeMap

instance Applicative Free where
    pure = pure'
    (<*>) = cata (fmap, (roll.))

mu :: Free (Free a) -> Free a
mu = cata (id, roll)

instance Monad Free where
    return = pure
    x >>= f = f =<< x where (=<<) = (mu.).fmap
-}
