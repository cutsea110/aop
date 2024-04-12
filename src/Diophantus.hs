module Diophantus where

euclid :: (Integral a) => (a, a) -> (a, a, a)
euclid (a, b) = _euclid (a, 1, 0) (b, 0, 1)
  where
    _euclid (a, x1, y1) (b, x2, y2)
      | b == 0    = (a, x1, y1)
      | otherwise = _euclid (b, x2, y2) (a - d * b, x1 - d * x2, y1 - d * y2)
      where
        d = a `div` b

diophantus :: (Integral a) => (a, a, a) -> Maybe (a, a)
diophantus (a, b, c)
  | m' == 0   = Just (x' * c', y' * c')
  | otherwise = Nothing
  where
    (d, x', y') = euclid (a, b)
    (c', m') = c `divMod` d

step :: (Integral a) => (a, a) -> (a, a)
step (a, b) = (b', -a')
  where
    d = gcd a b
    (a', b') = (a `div` d, b `div` d)
