{-# LANGUAGE ExplicitForAll, TypeOperators, RankNTypes #-}
-- | ref.) https://bartoszmilewski.com/2013/05/15/understanding-yoneda/
--   ref.) http://blog.sigfpe.com/2006/11/yoneda-lemma.html
--
module Milewski where

type f :-> g = forall a. f a -> g a

imager :: ((->) Bool) :-> []

data Color = Red | Green | Blue deriving Show
data Note  = C | D | E | F | G | A | B deriving Show

colorMap x = if x then Blue else Red
heatMap  x = if x then 32 else 212
soundMap x = if x then C else G

idBool :: Bool -> Bool
idBool x = x

-- suggested tests
test1 = imager colorMap

test2 = imager heatMap

test3 = imager soundMap

internalBools = [True, False, True, True]
imager iffie = fmap iffie internalBools

test = internalBools == imager idBool
