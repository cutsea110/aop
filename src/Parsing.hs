module Parsing
  ( Parser (Parser), apply, parse,
    none, fail_,
    natural, nat,
    many, some,
    paren,
    somewith, manywith,
    (<|>), guard,
    token, symbol,
    sat,
    space,
    digit,
    upto,
    int,
  )where


import Data.Char (isLower, isDigit, isSpace)

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) = p

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Functor Parser where
  fmap f (Parser p)  = Parser (\s -> [(f x, s') | (x, s') <- p s])

instance Applicative Parser where
  pure x = Parser (\s -> pure (x, s))
  Parser f <*> Parser x = Parser (\s -> [ (f' x', s'')
                                        | (f', s') <- f s
                                        , (x', s'') <- x s'])

instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  p >>= q = Parser (\s -> [ (y, s'')
                          | (x, s') <- apply p s
                          , (y, s'') <- apply (q x) s'])

getc :: Parser Char
getc = Parser f
  where f [] = []
        f (c:cs) = [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- getc; if p c then return c else fail_}

fail_ :: Parser a
fail_ = Parser (const [])

guard :: Bool -> Parser ()
guard True = return ()
guard False = fail_

char :: Char -> Parser ()
char x = do {c <- sat (==x); return ()}

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do {d <- sat isDigit; return (cvt d)}
  where cvt d = fromEnum d - fromEnum '0'

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f
  where f s = let ps = apply p s in
                if null ps then apply q s
                else ps

lowers :: Parser String
lowers = many lower

addition :: Parser Int
addition = do {m <- digit; char '+'; n <- digit; return (m+n)}

many :: Parser a -> Parser [a]
many p = optional (some p)

none :: Parser [a]
none = return []

space :: Parser ()
space = many (sat isSpace) >> return ()

symbol :: String -> Parser ()
symbol xs = space >> string xs

token :: Parser a -> Parser a
token p = space >> p

some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}

optional :: Parser [a] -> Parser [a]
optional p = p <|> none

natural :: Parser Int
natural = token nat

nat :: Parser Int
nat = do { ds <- some digit
         ; return (foldl1 shiftl ds)
         }
  where shiftl m n = 10 * m + n

int :: Parser Int
int = do {space; f <- minus; n <- nat; return (f n)}
  where minus = (char '-' >> return negate) <|> return id

ints :: Parser [Int]
ints = bracket (manywith (symbol ",") int)

bracket :: Parser a -> Parser a
bracket p = do { symbol "["
               ; x <- p
               ; symbol "]"
               ; return x
               }

manywith :: Parser b -> Parser a -> Parser [a]
manywith q p = optional (somewith q p)

somewith :: Parser b -> Parser a -> Parser [a]
somewith q p = do { x <- p
                  ; xs <- many (q >> p)
                  ; return (x:xs)
                  }

paren :: Parser a -> Parser a
paren p = do { symbol "("
             ; x <- p
             ; symbol ")"
             ; return x
             }

upto :: Char -> Parser String
upto c = Parser (\s -> let (xs, ys) = break (== c) s
                       in if null ys then []
                          else [(xs, tail ys)])

-----
{--
data Expr = Con Int | Bin Op Expr Expr -- deriving Show
data Op = Plus | Minus | Mul | Div -- deriving Show

instance Show Expr where
  showsPrec p (Con n) = showString (show n)
  showsPrec p (Bin op e1 e2) = showParen (p>q) (showsPrec q e1 . showSpace . shows op . showSpace . showsPrec (q+1) e2)
    where q = prec op
  -- unnecessary
  show e = showsPrec 0 e ""

instance Show Op where
  showsPrec _ Plus = showChar '+'
  showsPrec _ Minus = showChar '-'
  showsPrec _ Mul = showChar '*'
  showsPrec _ Div = showChar '/'

prec :: Op -> Int
prec Mul = 2
prec Div = 2
prec Plus = 1
prec Minus = 1

showSpace :: ShowS
showSpace = showChar ' '

expr :: Parser Expr
expr = token (term >>= rest)

rest :: Expr -> Parser Expr
rest e1 = do { p <- addop
             ; e2 <- term
             ; rest (Bin p e1 e2)
             } <|> return e1

constant :: Parser Expr
constant = do {n <- nat; return (Con n)}

binary :: Parser Expr
binary = do { e1 <- expr
            ; p <- op
            ; e2 <- term
            ; return (Bin p e1 e2)
            }

term :: Parser Expr
term = token (factor >>= more)

more :: Expr -> Parser Expr
more e1 = do { p <- mulop
             ; e2 <- factor
             ; more (Bin p e1 e2)
             } <|> return e1

factor :: Parser Expr
factor = token (constant <|> paren expr)

op :: Parser Op
op = (symbol "+" >> return Plus)
     <|> (symbol "-" >> return Minus)

addop, mulop :: Parser Op  
addop = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)
mulop = (symbol "*" >> return Mul) <|> (symbol "/" >> return Div)

float :: Parser Float
float = do { ds <- some digit
           ; char '.'
           ; fs <- some digit
           ; return (foldl shiftl 0 ds + foldr shiftr 0 fs)
           }
  where shiftl n d = 10 * n + fromIntegral d
        shiftr f x = (fromIntegral f + x) / 10


pair :: Parser a -> Parser b -> Parser (a, b)
pair p q = do { e1 <- p
              ; e2 <- q
              ; return (e1, e2)
              }

shunt :: Expr -> (Op, Expr) -> Expr
shunt e1 (op, e2) = Bin op e1 e2

shuntR :: (Op, Expr) -> (Expr -> Expr) -> Expr -> Expr
shuntR (op, e2) f1 e1 = Bin op e1 (f1 e2)

expr' :: Parser Expr
expr' = do { e1 <- term
           ; pes <- many (pair op term)
           -- ; return (foldl shunt e1 pes)
           ; return (foldr shuntR id pes e1) -- 右結合の演算にしたいケース
           }
--}
