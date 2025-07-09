{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module RLE where

import Data.Fix (Fix(..))
import Control.Monad.Free (Free(..), liftF)
import Data.Functor.Foldable (futu, cata, ListF(..))

-- coalgebra
rleCoalg :: String -> ListF (Char, Int) (Free (ListF (Char, Int)) String)
rleCoalg "" = Nil
rleCoalg (c:cs) = Cons (c, 1+length same) $ return rest
  where (same, rest) = span (==c) cs

-- encode
toRLE :: String -> Fix (ListF (Char, Int))
toRLE = futu rleCoalg

-- decode
fromRLE :: Fix (ListF (Char, Int)) -> [(Char, Int)]
fromRLE = cata phi
  where
    phi Nil         = []
    phi (Cons x xs) = x:xs

test :: String -> [(Char, Int)]
test = fromRLE . toRLE
