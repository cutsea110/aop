module Pretty where

import Prelude hiding ((<>))

type Layout = String
-- type Doc = [Layout]
data Doc = Nil
         | Line
         | Text String
         | Nest Int Doc
         | Group Doc
         | Doc :<>: Doc

nil = Nil
line = Line
text s = Text s
nest i x = Nest i x
group x = Group x
x <> y = x :<>: y

pretty :: Int -> Doc -> Layout
pretty = prettyFast

prettyFast w x = best w [(0, x)]
  where
    best r [] = ""
    best r ((i, x :<>: y):ids) = best r ((i, x) : (i, y) : ids)
    best r ((i, Nil):ids) = best r ids
    best r ((i, Line):ids) = '\n' : replicate i ' ' ++ best (w - i) ids
    best r ((i, Text s):ids) = s ++ best (r - length s) ids
    best r ((i, Nest j x):ids) = best r ((i+j,x):ids)
    best r ((i, Group x):ids) = better r (best r ((i, flatten x):ids)) (best r ((i, x):ids))

better r lx ly = if fits r lx then lx else ly

fits r _ | r < 0 = False
fits r [] = True
fits r (c:cs) = if c == '\n' then True
                else fits (r-1) cs

prettyVerySlow :: Int -> [String] -> String
prettyVerySlow w = fst . foldr1 choose . map augment
  where
    augment lx = (lx, shape lx)
    choose alx aly = if better (snd alx) (snd aly) then alx else aly
    better [] ks = True
    better js [] = False
    better (j:js) (k:ks)
      | j == k = better js ks
      | otherwise = j <= w

layouts :: Doc -> [Layout]
-- layouts = id
layouts (x :<>: y) = layouts x <++> layouts y
layouts Nil = [""]
layouts Line = ["\n"]
layouts (Text s) = [s]
layouts (Nest i x) = map (nestl i) (layouts x)
layouts (Group x) = layouts (flatten x) ++ layouts x

-- (<>) :: Doc -> Doc -> Doc
-- (<>) = (<++>)

-- nil :: Doc
-- nil = [""]

-- text :: String -> Doc
-- text s = [s]

-- line :: Doc
-- line = ["\n"]

-- nest :: Int -> Doc -> Doc
-- nest i = map (nestl i)

-- group :: Doc -> Doc
-- group x = flatten x ++ x

(<++>) :: [Layout] -> [Layout] -> [Layout]
xss <++> yss = [xs ++ ys | xs <- xss, ys <- yss]

nestl :: Int -> Layout -> Layout
nestl i = concat . map (indent i)

indent :: Int -> Char -> String
indent i c = if c == '\n' then c : replicate i ' ' else [c]

flatten :: Doc -> Doc
-- flatten x = [flattenl (head x)]
flatten (x :<>: y) = flatten x :<>: flatten y
flatten Nil = Nil
flatten Line = Text " "
flatten (Text s) = Text s
flatten (Nest i x) = flatten x
flatten (Group x) = flatten x

toDoc :: [(Int, Doc)] -> Doc
toDoc ids = foldr (:<>:) Nil [Nest i x | (i, x) <- ids]

{--
layr = layouts . toDoc
layr [] = [""]
layr ((i, x :<>: y):ids) = layr ((i, x) : (i, y) : ids)
layr ((i, Nil):ids) = layr ids
layr ((i, Line):ids) = ['\n':replicate i ' ' ++ ls | ls <- layr ids]
layr ((i, Text s):ids) = [s ++ ls | ls <- layr ids]
layr ((i, Nest j x):ids) = layr ((i + j, x) : ids)
layr ((i, Group x):ids) = layr ((i, flatten x) : ids) ++ layr ((i, x) : ids)
--}

flattenl :: Layout -> Layout
flattenl [] = []
flattenl (c:cs)
  | c == '\n' = ' ' : flattenl (dropWhile (==' ') cs)
  | otherwise = c : flattenl cs

shape :: Layout -> [Int]
shape = map length . lines

para :: String -> Doc
para = cvt . map text . words

cvt :: [Doc] -> Doc
cvt [] = nil
cvt (x:ys) = x <> foldr (<>) nil [group (line <> y) | y <- ys]

pg :: String
pg = "This is a fairly short paragraph with just twenty-two words. The problem is that pretty-printing it take time, in fact 40.83 seconds."

