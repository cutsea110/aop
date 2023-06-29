module Parser where

import Data.Char (isDigit, isAlpha, isAlphaNum)

type Location = Int
type Token = (Location, String)
newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])] }

pSat :: (String -> Bool) -> Parser String
pSat p = Parser f
  where
    f [] = []
    f ((_, tok):toks) = if p tok then [(tok, toks)] else []

pLit :: String -> Parser String
pLit s = pSat (== s)

pApply :: Parser a -> (a -> b) -> Parser b
p `pApply` f = Parser (\toks -> [(f v, toks') | (v, toks') <- runParser p toks])

pNum :: Parser Int
pNum = pSat (all isDigit) `pApply` read

pAlt :: Parser a -> Parser a -> Parser a
p `pAlt` q = Parser (\toks -> runParser p toks ++ runParser q toks)

pAltL :: Parser a -> Parser a -> Parser a
p `pAltL` q = Parser (\toks -> runParser p toks <+ runParser q toks)
  where
    [] <+ ys = ys
    xs <+ _ = xs

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen op p q = Parser f
  where
    f toks = [ (x `op` y, toks'')
             | (x, toks') <- runParser p toks
             , (y, toks'') <- runParser q toks'
             ]

pVar :: Parser String
pVar = pSat p
  where
    p (c:cs) = isAlpha c && all isAlphaNum cs
    p [] = error "Empty string"

pEmpty :: a -> Parser a
pEmpty x = Parser (\toks -> [(x, toks)])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pOneOrMoreWithSep :: Parser sep -> Parser a -> Parser [a]
pOneOrMoreWithSep sep p = pThen (:) p (pZeroOrMore (pThen (flip const) sep p))

pMunch1 :: Parser a -> Parser [a]
pMunch1 p = pThen (:) p (pMunch p)

pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAltL` pEmpty []
