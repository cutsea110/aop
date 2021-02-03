-- 逆により仕様
--
unzip' [] = ([], [])
unzip' ((a,b):xys) = (a:xs, b:ys) where (xs, ys) = unzip' xys

zip' ([],[]) = []
zip' (a:xs, b:ys) = (a,b):xys where xys = zip' (xs,ys)
