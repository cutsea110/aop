-- | ref.) http://okmij.org/ftp/Haskell/numbered-monad.txt
{-# LANGUAGE TupleSections #-}
module SNMonad where

type Numbered a = (Int, a)

newtype NumberedM a = NumberedM { runNumberedM :: Int -> Numbered a }

instance Functor NumberedM where
  fmap f (NumberedM m)
    = NumberedM $ \n -> let (n', v) = m n
                        in (n', f v)

instance Applicative NumberedM where
  pure x = NumberedM (,x)
  -- ^ m :: Int -> (Int, a)
  -- ^ f :: Int -> (Int, a -> b)
  NumberedM f <*> NumberedM m
    = NumberedM $ \n -> let (n1, v)   = m n
                            (n2, f1) = f n1
                        in (n2, f1 v)

instance Monad NumberedM where
  NumberedM m >>= f
    = NumberedM $ \n -> let (n1, v) = m n
                            NumberedM m1 = f v
                        in m1 n1
  -- return x = NumberedM (,x)

incr :: NumberedM Int
incr = NumberedM $ \n -> (n+1, n)

----
-- application

data Tree a = Nd a (Forest a) deriving Show
type Forest a = [Tree a]

makeNode :: a -> Forest (Int, a) -> NumberedM (Tree (Int, a))
makeNode val kids = do
  { n <- incr
  ; return (Nd (n, val) kids)
  }


{- | makeBtree
>>> runNumberedM (makeBtree 3) 100
(115,Nd (114,3) [Nd (106,2) [Nd (102,1) [Nd (100,0) [],Nd (101,0) []],Nd (105,1) [Nd (103,0) [],Nd (104,0) []]],Nd (113,2) [Nd (109,1) [Nd (107,0) [],Nd (108,0) []],Nd (112,1) [Nd (110,0) [],Nd (111,0) []]]])
-}
makeBtree :: Int -> NumberedM (Tree (Numbered Int))
makeBtree 0 = makeNode 0 []
makeBtree depth = do
  { left  <- makeBtree (depth - 1)
  ; right <- makeBtree (depth - 1)
  ; makeNode depth [left, right]
  }


----

type Numberedlike a = (Int, a)
newtype NumberedlikeM a = NumberedlikeM { runNumberedlikeM :: Int -> Numberedlike a }

ret :: a -> NumberedlikeM a
ret x = NumberedlikeM $ \n -> (n, x)

app :: (a -> NumberedlikeM b) -> NumberedlikeM a -> NumberedlikeM b
app f (NumberedlikeM m)
  = NumberedlikeM $ \n -> let (n1, v) = m n
                              NumberedlikeM m1 = f v
                          in m1 n1

m >>- f = app f m

incrlike :: NumberedlikeM Int
incrlike = NumberedlikeM $ \n -> (n+1, n)

makeNode' :: a -> Forest (Int, a) -> NumberedlikeM (Tree (Int, a))
makeNode' val kids = incrlike >>- (\n -> ret (Nd (n, val) kids))

{- | makeBtree'
>>> runNumberedlikeM (makeBtree' 3) 100
(115,Nd (114,3) [Nd (106,2) [Nd (102,1) [Nd (100,0) [],Nd (101,0) []],Nd (105,1) [Nd (103,0) [],Nd (104,0) []]],Nd (113,2) [Nd (109,1) [Nd (107,0) [],Nd (108,0) []],Nd (112,1) [Nd (110,0) [],Nd (111,0) []]]])
-}
makeBtree' :: Int -> NumberedlikeM (Tree (Numberedlike Int))
makeBtree' 0 = makeNode' 0 []
makeBtree' depth = makeBtree' (depth-1) >>-
                   (\left -> makeBtree' (depth-1) >>-
                     (\right -> makeNode' depth [left, right]))
