{-# LANGUAGE TupleSections, LambdaCase #-}
module MapWhile where

mapWhile1 f = u
  where u [] = []
        u (x:xs) = g (f x, u xs)
        g (b, bs) = maybe [] (:bs) b


cata (c, f) = u
  where u [] = c
        u (x:xs) = f (x, u xs)

mapWhile2 f = cata ([], g)
  where g (b, bs) = maybe [] (:bs) (f b)


apo psi = v
  where v b = case psi b of
          Nothing      -> []
          Just (b, bs) -> b:either id v bs

mapWhile3 f = apo $ \case
  []   -> Nothing
  a:as -> g (f a, Right as)
  where g (b, bs) = fmap (,bs) b

ana psi = v
  where v x = case psi x of
          Nothing      -> []
          Just (b, x)  -> b:v x

mapWhile4 f = ana $ \case
  []   -> Nothing
  a:as -> g (f a, as)
  where g (b, bs) = fmap (,bs) b
