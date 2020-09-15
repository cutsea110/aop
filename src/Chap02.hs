module Chap02 where

-- | Ex 2.1
-- f . u = f
--        f          u
-- B <------- A <-------- A
-- B <------------------- A
--            f
--
-- f は任意なので f として id_A をとる
--
--  id_A . u
-- == {- f . u = f -}
--  id_A
--
-- 一方 id_A は単位元なので
--  id_A . u
-- == {- id_A は単位元 -}
--  u
--
-- ゆえに id_A . u == id_A かつ id_A . u == u なので u == id_A
--

