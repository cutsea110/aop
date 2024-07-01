module PHOAS where

-- Keiigo Imai's article 2008-12-26
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

-- HOAS なid関数
idH :: TermH
idH = LamH (\x -> x)

testHOAS :: TermH
testHOAS = AppH (LamH $ \f -> AppH f (ConH 1)) (LamH $ \x -> x)


-- HOAS その2
