{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module MonadicRecursionScheme where

import           Control.Monad              ((<=<), liftM2)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Functor.Foldable      (Recursive (..), Corecursive (..), Base, Fix (..))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map


data ExprF r = VarF String
             | LitF Int
             | AddF r r
             deriving (Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF

var :: String -> Expr
var = Fix . VarF

lit :: Int -> Expr
lit = Fix . LitF

add :: Expr -> Expr -> Expr
add a b = Fix (AddF a b)

{--
eval :: Expr -> Int
eval = cata $ \case
  LitF j -> j
  AddF i j -> i + j
  VarF _ -> error "free variable in expression"

monadicAlgebra = \case
  LitF j   -> return j
  AddF i j -> return (i + j)
  VarF v   -> Left (FreeVar v)
--}

data Error =
  FreeVar String
  deriving Show

cataM :: (Monad m, Traversable (Base t), Recursive t)
      => (Base t a -> m a) -> t -> m a
cataM alg = h
  where h = alg <=< traverse h . project

{--
eval :: Expr -> Either Error Int
eval = cataM $ \case
  LitF j   -> return j
  AddF i j -> return (i + j)
  VarF v   -> Left (FreeVar v)
--}

eval :: Expr -> ReaderT (Map String Int) (Either Error) Int
eval = cataM $ \case
  LitF j   -> return j
  AddF i j -> return (i + j)
  VarF v   -> do
    env <- ask
    case Map.lookup v env of
      Nothing -> lift (Left (FreeVar v))
      Just j  -> return j
