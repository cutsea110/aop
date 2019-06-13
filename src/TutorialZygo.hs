module TutorialZygo where
-- Ref.) https://stackoverflow.com/questions/36851766/histomorphisms-zygomorphisms-and-futumorphisms-specialised-to-lists
--      Benjamin Hodgson's answer

lengthEven :: [a] -> Bool
lengthEven = even . length

pm0 [] = 0
pm0 (x:xs) = if lengthEven xs then x - pm0 xs else x + pm0 xs

paraL f z [] = z
paraL f z (x:xs) = f x xs (paraL f z xs)

pm1 = paraL (\x xs acc -> if lengthEven xs then x - acc else x + acc) 0

cataL :: (a -> b -> b) -> b -> [a] -> b
cataL = foldr

lengthEven' = cataL (\_ p -> not p) True
paraL' f z = snd . cataL (\x (xs, acc) -> (x:xs, f x xs acc)) ([], z)

-- performance is good!
-- for example, you check for [30000,29999..1] between pm1 to pm2
pm2 = snd . cataL (\x (isEven, total) -> (not isEven, if isEven then x - total else x + total)) (True, 0)

zygoL :: (a -> b -> b) -> -- a folding function
         (a -> b -> c -> c) -> -- a folding function which depends on the result of the other fold
         b -> c -> -- zeroes for the two folds
         [a] -> c
zygoL f g z e = snd . cataL (\x (p, q) -> (f x p, g x p q)) (z, e)

pm3 = zygoL (\_ p -> not p) (\x isEven total -> if isEven then x - total else x + total) True 0


-- zygo has a big sister called mutumorphism
mutuL :: (a -> b -> c -> b) ->
         (a -> b -> c -> c) ->
         b -> c ->
         [a] -> c
mutuL f g z e = snd . cataL (\x (p, q) -> (f x p q, g x p q)) (z, e)

pm4 = mutuL (\_ p _ -> not p) (\x isEven total -> if isEven then x - total else x + total) True 0
