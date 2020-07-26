{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Curry where

import Prelude hiding (null, pred, const, foldr, lines)
import Data.Void
import Data.List (unfoldr)

type Boolean = Either () ()
true  = Right ()
false = Left ()

assocr :: ((a, b), c) -> (a, (b, c))
assocr ((x, y), z) = (x, (y, z))

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (x, (y, z)) = ((x, y), z)

distr :: (a, Either b c) -> Either (a, b) (a, c)
distr (x, Left  y) = Left  (x, y)
distr (x, Right z) = Right (x, z)

undistr :: Either (a, b) (a, c) -> (a, Either b c)
undistr (Left  (x, y)) = (x, Left  y)
undistr (Right (x, z)) = (x, Right z)

distl :: (Either a b, c) -> Either (a, c) (b, c)
distl (Left  x, z) = Left  (x, z)
distl (Right y, z) = Right (y, z)

undistl :: Either (a, c) (b, c) -> (Either a b, c)
undistl (Left  (x, z)) = (Left  x, z)
undistl (Right (y, z)) = (Right y, z)

unit :: (a, ()) -> a
unit (x, ()) = x

void :: Void
void = undefined

unnull :: Void -> (a, Void)
unnull = (, void) . absurd

null :: (a, Void) -> Void
null (_, a) = a


pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)
cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) (x, y) = (f x, g y)

-- p?
bool :: (a -> Boolean) -> a -> Either a a
bool p = either (Left . unit) (Right . unit) . distr . pair (id, p)

pred_test = map (bool isEven) [1..10]
  where isEven x = if even x then true else false

conditional :: (a -> Boolean) -> (a -> b) -> (a -> b) -> a -> b
conditional p f g = either f g . bool p

const :: a -> b -> a
const f _ = f

compose :: (b -> c, a -> b) -> a -> c
compose (f, g) = f . g

nil :: () -> [a]
nil () = []
cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs

foldr :: (b, (a, b) -> b) -> [a] -> b
foldr (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

ccons :: a -> [a] -> [a]
ccons = (:)

outl :: (a, b) -> a
outl = fst
outr :: (a, b) -> b
outr = snd

apply :: (b -> a, b) -> a
apply (f, x) = f x

ccat :: [a] -> [a] -> [a]
ccat = foldr (id, compose . cross (ccons, id))

phi :: (Either () (a, [a]), [a]) -> Either ((), [a]) (a, ([a], [a]))
phi = either Left (Right . assocr) . distl

-- B == [a]
-- B^B == [a] -> [a]
-- k :: () + a * B^B --> B^B
-- k :: Either () (a, [a] -> [a]) -> [a] -> [a]
-- k = curry $ either outr (cons . cross (id, apply) . assocr) . distl

k :: Either () (a, [a] -> [a]) -> [a] -> Either [a] [a]
k = curry $ either (Left . outr) (Right . cons . cross (id, apply) . assocr) . distl

-- >>> k (Left ()) [1,2,3]
-- Left [1,2,3]
--
-- >>> k (Right (1, ([2,3]++))) [4,5,6]
-- Right [1,2,3,4,5,6]

cat []     = k (Left ())
cat (x:xs) = k (Right (x, (xs++)))

k' :: Either () (a, [a] -> [a]) -> [a] -> [a]
k' = curry $ either outr (cons . cross (id, apply) . assocr) . distl
cat' []     = k' (Left ())
cat' (x:xs) = k' (Right (x,(cat' xs)))

lines s = case break (=='\n') s of
  (ps,   []) -> ps : []
  (ps, _:qs) -> ps : lines qs
