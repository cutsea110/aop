module Diophantus where

euclid :: (Integral a) => (a, a) -> (a, a, a)
euclid (a, b) = go (a, 1, 0) (b, 0, 1)
  where
    go u@(a, x1, y1) v@(b, x2, y2)
      | b == 0    = u
      | otherwise = go v (a', x1', y1')
      where
        d = a `div` b
        a' = a - d * b
        x1' = x1 - d * x2
        y1' = y1 - d * y2

diophantus :: (Integral a) => (a, a, a) -> Maybe (a, a)
diophantus (a, b, c)
  | m' == 0   = Just (x' * c', y' * c')
  | otherwise = Nothing
  where
    (d, x', y') = euclid (a, b)
    (c', m') = c `divMod` d

diophantus' :: (Integral a) => (a, a, a) -> Maybe ((a, a), (a, a))
diophantus' (a, b, c) = do
  xy <- diophantus (a, b, c)
  return (xy, step (a, b))
  where
    step (a, b) = (b', -a')
      where
        d = gcd a b
        (a', b') = (a `div` d, b `div` d)
