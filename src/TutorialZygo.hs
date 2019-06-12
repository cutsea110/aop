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
