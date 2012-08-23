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

data Regex = Epsilon
           | RChar       Char
           | Concat     Regex Regex
           | And        Regex Regex
           | Or         Regex Regex
           | Repetition Regex
           | Negation   Regex
           | Empty
           deriving (Show)

nullable :: Regex -> Bool
nullable Epsilon        = True
nullable (RChar _)      = False
nullable (Concat r1 r2) = nullable r1 && nullable r2
nullable (And r1 r2)    = nullable r1 && nullable r2
nullable (Or r1 r2)     = nullable r1 || nullable r2
nullable (Repetition r) = True
nullable (Negation r)   = not $ nullable r
nullable Empty          = False

regexNullable :: Regex -> Regex
regexNullable regex | nullable regex = Epsilon
                    | otherwise      = Empty

derivative :: Char -> Regex -> Regex
derivative char Epsilon = Empty
derivative char1 (RChar char2) =
    if char1 == char2
      then Epsilon
      else Empty
derivative char (Concat regex1 regex2) = (Or (Concat (derivative char regex1) regex2)
                                             (Concat (regexNullable regex1) (derivative char regex2)))
derivative char (And regex1 regex2) = (And      (derivative char regex1) (derivative char regex2))
derivative char (Or regex1 regex2)  = (Or       (derivative char regex1) (derivative char regex2))
derivative char (Repetition regex)  = (Concat   (derivative char regex)  (Repetition regex))
derivative char (Negation regex)    = (Negation (derivative char regex))
derivative char Empty               = Empty

matches :: Regex -> String -> Bool
regex `matches` "" = nullable regex
regex `matches` (char:rest) = (derivative char regex) `matches` rest
