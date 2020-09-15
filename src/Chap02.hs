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
--  id_A
-- == {- f . u = f -}
--  id_A . u
-- == {- id_A は単位元 -}
--  u
-- よって id_A = u

