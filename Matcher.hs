module Matcher where

data Regex = Epsilon
           | RChar      Char
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
derivative char (Or  regex1 regex2) = (Or       (derivative char regex1) (derivative char regex2))
derivative char (Repetition regex)  = (Concat   (derivative char regex)  (Repetition regex))
derivative char (Negation   regex)  = (Negation (derivative char regex))
derivative char Empty               = Empty

matches :: Regex -> String -> Bool
regex `matches` "" = nullable regex
regex `matches` (char:rest) = (derivative char regex) `matches` rest
