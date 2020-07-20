{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Curry where

import Prelude hiding (null)
import Data.Void

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

unnull :: Void -> (a, Void)
unnull = (, undefined::Void) . absurd

null :: (a, Void) -> Void
null (_, a) = a


