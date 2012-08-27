module Parser where

import Control.Monad

newtype Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c,cs)])

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    parser >>= f = Parser (\cs -> concat [parse (f a) cs' |
                                          (a, cs') <- parse parser cs])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                           []     -> []
                           (x:xs) -> [x])

sat :: (Char -> Bool) -> Parser Char
sat predicate = do
    char <- item
    if predicate char
      then return char
      else mzero

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

many :: Parser a -> Parser [a]
many parser = many1 parser +++ return []

many1 :: Parser a -> Parser [a]
many1 parser = do
    a <- parser
    as <- many parser
    return (a:as)

sepby :: Parser a -> Parser b -> Parser [a]
parser `sepby` sep = (parser `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
parser `sepby1` sep = do
    a <- parser
    as <- many (sep >> parser)
    return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 +++ return a
