module MapWhile where

cata (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

mapWhile f = cata ([], g)
  where g (b, bs) = maybe [] (:bs) (f b)
