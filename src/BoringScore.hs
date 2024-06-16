module BoringScore where

scores :: [Int] -> [Int]
scores ts = take 10 $ go ts []
  where
    go :: [Int] -> [Int] -> [Int]
    go []       acc = reverse acc
    go (10:ts)  acc = go ts (ttl acc + 10 + sum (take 2 ts) : acc)
    go (x:y:ts) acc
      | x + y == 10 = go ts (ttl acc + 10 + head ts : acc)
      | otherwise   = go ts (ttl acc + x + y : acc)
    go (x:[])   acc = go [] (ttl acc + x : acc)

    ttl [] = 0
    ttl (x:_) = x

test:: [Int]
test = [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]

perfect:: [Int]
perfect = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]

data Throws = Strike
            | Spare Int
            | Pair Int Int
            | Open Int
            deriving (Show)

type Frame = Int

throws :: [Int] -> (Frame, [Throws])
throws ps = go ps 0 []
  where
    go [] n acc = (n, reverse acc)
    go xs n acc = case xs of
      10:zs -> go zs (n+1) (Strike:acc)
      x:y:ys
        | x + y == 10 -> go ys (n+1) (Spare x:acc)
        | otherwise   -> go ys (n+1) (Pair x y:acc)
      x:[] -> go [] (n+1) (Open x:acc)

score' :: [Throws] -> [Int]
score' ts = take 10 $ go ts []
  where
    go :: [Throws] -> [Int] -> [Int]
    go []       acc = reverse acc
    go (Strike:ts)  acc = go ts (ttl acc + 10 + nextTwo ts : acc)
    go (Spare x:ts) acc = go ts (ttl acc + 10 + nextOne ts : acc)
    go (Pair x y:ts) acc = go ts (ttl acc + x + y : acc)
    go (Open x:ts) acc = go ts (ttl acc + x : acc)

    ttl [] = 0
    ttl (x:_) = x

    nextOne (Strike:_) = 10
    nextOne (Spare x:_) = x
    nextOne (Pair x y:_) = x
    nextOne (Open x:_) = x

    nextTwo (Strike:Strike:_) = 10
    nextTwo (Strike:Spare x:_) = 10 + x
    nextTwo (Strike:Pair x y:_) = 10 + x
    nextTwo (Strike:Open x:_) = 10 + x
    nextTwo (Spare x:_) = 10
    nextTwo (Pair x y:_) = x + y
    nextTwo (Open x:_) = x
