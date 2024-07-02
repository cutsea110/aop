{-# LANGUAGE RankNTypes, GADTs #-}
module PhoasRyan where

-- ref.) https://keigoi.hatenadiary.org/entry/20081225/p1
-- ref.) https://www.mail-archive.com/haskell-cafe@haskell.org/msg48913.html

import Control.Monad.Reader

data ExpP v t where
  VarP :: v t -> ExpP v t
  AppP :: ExpP v (a -> b) -> ExpP v a -> ExpP v b
  LamP :: (v a -> ExpP v b) -> ExpP v (a -> b)

newtype Exp t = Exp (forall v. ExpP v t)

eval :: Exp t -> t
eval (Exp e) = evalP e

-- PHOAS 項の評価
newtype Prim a = Prim a
-- 単に剥いてホスト言語の関数適用を利用
evalP :: ExpP Prim t -> t
evalP (VarP (Prim a)) = a
evalP (AppP e1 e2) = evalP e1 $ evalP e2
evalP (LamP f) = evalP . f . Prim


-- PHOAS 項の表示
-- 簡単に de Bruijn indices で表現
newtype Var a = Var Int
-- reader モナドを使って fresh な変数を供給
showExp :: ExpP Var a -> ShowS
showExp t = runReader (showExpR t) 0
  where
    showExpR :: ExpP Var a -> Reader Int ShowS
    showExpR (VarP (Var i)) = return (shows i)
    showExpR (AppP e1 e2) = return $ \s -> showExp e1 (" " ++ showExp e2 s)
    showExpR (LamP f) = do
      i <- ask
      fresh <- local (+1) (showExpR (f (Var i)))
      return $ \s -> "\\" ++ shows i "-> " ++ fresh s

{- |
>>> evalP test (+1) 2
3
>>> putStrLn $ showExp test ""
\0-> \1-> 0 1
>>> eval (Exp test) (+1) 2
3
-}
test :: ExpP v ((a -> b) -> a -> b)
test = LamP $ \x -> LamP $ \y -> AppP (VarP x) (VarP y) 
