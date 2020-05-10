{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module MonadicRecursionScheme where

import           Control.Comonad            (Comonad (..))
import           Control.Comonad.Cofree     (Cofree (..))
import           Control.Monad              ((<=<), liftM2)
import           Control.Monad.Free         (Free (..))
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

cataM :: (Monad m, Traversable (Base t), Recursive t)
      => (Base t a -> m a) -> t -> m a
cataM phi = h
  where h = phi <=< mapM h . project

anaM :: (Monad m, Traversable (Base t), Corecursive t)
     => (a -> m (Base t a)) -> a -> m t
anaM psi = h
  where h = (return . embed) <=< mapM h <=< psi

paraM :: (Monad m, Traversable (Base t), Recursive t)
      => (Base t (t, a) -> m a) -> t -> m a
paraM phi = h
  where h = phi <=< mapM (liftM2 (,) <$> return <*> h) . project

apoM :: (Monad m, Traversable (Base t), Corecursive t)
     => (a -> m (Base t (Either t a))) -> a -> m t
apoM psi = h
  where h = (return . embed) <=< mapM (either return h) <=< psi

hyloM :: (Monad m, Traversable t)
      => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM phi psi = h
  where h = phi <=< mapM h <=< psi

histoM :: (Monad m, Traversable (Base t), Recursive t)
       => (Base t (Cofree (Base t) a) -> m a) -> t -> m a
histoM phi = return . extract <=< cataM f
  where f  = return . uncurry (:<) <=< (liftM2 (,) <$> phi <*> return)

futuM :: (Monad m, Traversable (Base t), Corecursive t)
      => (a -> m (Base t (Free (Base t) a))) -> a -> m t
futuM psi = anaM f . Pure
  where f (Pure  a) = psi a
        f (Free fb) = return fb
