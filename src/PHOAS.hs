{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             StrictData
#-}
module PHOAS where

-- Keigo Imai's article 2008-12-26
-- ref) https://keigoi.hatenadiary.org/entry/20081226/p1
--
import Control.Monad.Reader
import Data.Maybe

-- FOAS
data Term1 = Var String
           | Lam String Term1
           | App Term1 Term1
           | Con Int

-- FOAS な id 関数
testFOAS :: Term1
testFOAS = Lam "x" (Var "x")

badFOAS :: Term1
badFOAS = Lam "x" (Var "y")

-- HOAS その1
data TermH = LamH (TermH -> TermH)
           | AppH TermH TermH
           | ConH Int

-- エバってみる。簡単
evalH :: TermH -> TermH
evalH x@(LamH _) = x
evalH (AppH t1 t2) = case evalH t1 of
  LamH f -> evalH (f (evalH t2))
  ConH _ -> error "bad application"
evalH x@(ConH _) = x

-- HOAS の項を表示 (関数の中身はわからない)
instance Show TermH where
  show (LamH _) = "<fun>" -- λ抽象の中身はわからない
  show (AppH t1 t2) = show t1 ++ " " ++ show t2
  show (ConH n) = show n

{-- | HOAS なid関数
>>> idH
<fun>
--}
idH :: TermH
idH = LamH (\x -> x)

{-- | HOAS のテスト (\f -> f 1) (\x -> x) 相当
-- 中身は不明
>>> testHOAS
<fun> <fun>

-- エバるのは簡単
>>> evalH testHOAS
1
--}
testHOAS :: TermH
testHOAS = AppH (LamH $ \f -> AppH f (ConH 1)) (LamH $ \x -> x)


-- HOAS その2
newtype VarE = VarE Char
data TermH2 = LamH2 (VarE -> TermH2)
            | AppH2 TermH2 TermH2
            | ConH2 Int
            | VarH2 VarE

-- HOAS の項を表示 (関数の中身もわかる)
instance Show TermH2 where
  show t = runReader (showR t) 'a'
    where
      showR :: TermH2 -> Reader Char String
      showR (LamH2 f) = do
        fresh <- ask
        body <- local succ (showR (f (VarE fresh)))
        return $ "(\\" ++ [fresh] ++ " -> " ++ body ++ ")"

      showR (AppH2 t1 t2) = do
        t1' <- showR t1
        t2' <- showR t2
        return $ t1' ++ " " ++ t2'

      showR (ConH2 n) = return $ show n

      showR (VarH2 (VarE c)) = return [c]

-- エバってみる。変数のルックアップが必要。非現実的
evalH2 :: TermH2 -> TermH2
evalH2 t = runReader (evalH2R t) ([], 'a')
  where
    evalH2R (AppH2 t1 t2) = do
      t1' <- evalH2R t1
      t2' <- evalH2R t2
      (_, fresh) <- ask
      case t1' of
        LamH2 f ->
          local (\(env, var) -> ((fresh, t2'):env, succ var))
          $ evalH2R (f (VarE fresh))
        _ -> error "not function"

    evalH2R (VarH2 (VarE name)) = do
      (env, _) <- ask
      return $ fromJust $ lookup name env
      
    evalH2R x@(ConH2 _) = return x
    
    evalH2R x@(LamH2 _) = return x

{-- | HOAS2 のテスト (\f -> f 1) (\x -> x) 相当
-- 関数の中身が分かる
>>> testHOAS2
(\a -> a 1) (\a -> a)

>>> evalH2 testHOAS2
1
--}
testHOAS2 :: TermH2
testHOAS2 = AppH2 (LamH2 $ \f -> AppH2 (VarH2 f) (ConH2 1)) (LamH2 $ \x -> VarH2 x)

-- PHOAS -- HOAS2 の VarE を型変数 v でパラメタ化
data TermP v = VarP v
             | LamP (v -> TermP v)
             | AppP (TermP v) (TermP v)
             | ConP Int

instance Show (TermP VarE) where
  show t = runReader (showP t) 'a'
    where
      showP :: TermP VarE -> Reader Char String
      showP (VarP (VarE name)) = return [name]

      showP (LamP f) = do
        fresh <- ask
        let body = f (VarE fresh)
        bodystr <- local succ (showP body)
        return $ "(\\" ++ [fresh] ++ " -> " ++ bodystr ++ ")"

      showP (AppP t1 t2) = do
        t1s <- showP t1
        t2s <- showP t2
        return $ t1s ++ " " ++ t2s

      showP (ConP n) = return $ show n

newtype Id = Id (TermP Id)
evalP :: TermP Id -> Int
evalP t = case evalP' t of
  ConP n -> n
  _      -> error "not a number"
  where
    evalP' :: TermP Id -> TermP Id
    evalP' x@(LamP _) = x
    -- エバるときは項をそのままぶち込む
    evalP' (AppP t1 t2) = case evalP' t1 of
      LamP f -> evalP' (f (Id $ evalP' t2))
      ConP _ -> error "not a function"
    evalP' x@(ConP _) = x
    -- VarP (Id t) の t に項がそのまま入っているので変数ルックアップは必要なし
    evalP' x@(VarP (Id t)) = t

{-- | PHOASのテスト (\f -> f 1) (\x -> x) 相当
-- 関数の中身が分かる (VarE の方は Show のインスタンス宣言があるので
>>> testPHOAS :: TermP VarE
(\a -> a 1) (\a -> a)
-- 評価もできるが、こっちは TermP Id で使える evalP を実装しているから
>>> evalP testPHOAS
1
--}
testPHOAS :: TermP v
testPHOAS = AppP (LamP $ \f -> AppP (VarP f) (ConP 1)) (LamP $ \x -> VarP x)
