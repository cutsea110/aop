module MapWhile where

--               In                x     xs
--    [a] <---------------- 1 +    a    * [a]
--     |                      |
--   u |                      | id + f * u
--     |                      |
--     v                      v
--    [b] <---------------- 1 + (1 + b) * [b]
--           id + g               f x     u xs
--
--  u . In = (id + g) . (id + f * u) = id + g . (f * u)
--
mapWhile f = u
  where u [] = []
        u (x:xs) = g (f x, u xs)
        g (b, bs) = maybe [] (:bs) b
