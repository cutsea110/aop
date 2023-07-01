module Parser where

import Data.Char (isDigit, isSpace)

type Location = Int
type Token = (Location, String)
newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])]}

pSat :: (String -> Bool) -> Parser String
pSat p = Parser f
  where
    f [] = []
    f ((_, tok):toks) = if p tok then [(tok, toks)] else []

pLit :: String -> Parser String
pLit s = pSat (== s)

pEmpty :: a -> Parser a
pEmpty x = Parser (\toks -> [(x, toks)])

pBind :: Parser a -> (a -> Parser b) -> Parser b
px `pBind` f = Parser (\toks -> [ (v, toks'')
                                | (x, toks') <- runParser px toks
                                , (v, toks'') <- runParser (f x) toks'
                                ])

pApply :: (a -> b) -> Parser a -> Parser b
f `pApply` p = Parser (\toks -> [ (f x, toks')
                                | (x, toks') <- runParser p toks
                                ])
pNum :: Parser Int
pNum = read `pApply` pSat (all isDigit)

pAlt :: Parser a -> Parser a -> Parser a
p `pAlt` q = Parser (\toks -> runParser p toks ++ runParser q toks)

pAltL :: Parser a -> Parser a -> Parser a
p `pAltL` q = Parser (\toks -> runParser p toks <+ runParser q toks)
  where
    [] <+ ys = ys
    xs <+ _  = xs


pAp :: Parser (a -> b) -> Parser a -> Parser b
pf `pAp` px = Parser (\toks -> [ (f v, toks'')
                               | (f, toks') <- runParser pf toks
                               , (v, toks'') <- runParser px toks'
                               ])

pApply2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pApply2 f p q = Parser (\toks -> [ (f x y, toks'')
                                 | (x, toks') <- runParser p toks
                                 , (y, toks'') <- runParser q toks'
                                 ])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pApply2 (:) p (pZeroOrMore p)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pMunch1 :: Parser a -> Parser [a]
pMunch1 p = pApply2 (:) p (pMunch p)

pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAltL` pEmpty []

pOneOrMoreWithSep :: Parser sep -> Parser a -> Parser [a]
pOneOrMoreWithSep sep p = pApply2 (:) p (pZeroOrMore (sep *> p))

pMunch1WithSep :: Parser sep -> Parser a -> Parser [a]
pMunch1WithSep sep p = pApply2 (:) p (pMunch (sep *> p))

instance Functor Parser where
  fmap = pApply

instance Applicative Parser where
  pure = pEmpty
  (<*>) = pAp

instance Monad Parser where
  return = pure
  (>>=) = pBind

-------

fileName :: Parser String
fileName = concat <$> pMunch1 (pSat (/=":"))
mimeType :: Parser String
mimeType = concat <$> pMunch1 (pSat (/="\n"))
sep :: Parser ()
sep = () <$ (pLit ":" *> pMunch (pSat (all isSpace)))
line :: Parser (String, String)
line = (,) <$> fileName <* sep <*> mimeType
content = pMunch1WithSep (pLit "\n") line <* pMunch (pSat (all isSpace))

main :: IO [(String, String)]
main = do
  inp <- readFile "mime.txt"
  let toks = concatMap (\c -> [(1, [c])]) inp
  return . fst . head . runParser content $ toks
