module ParseLib.Parallel.Applications
  (
    -- * Applications of elementary parsers
    digit,
    newdigit,
    natural,
    integer,
    identifier,
    parenthesised,
    bracketed,
    braced,
    commaList,
    semiList
  )
  where

import Data.Char
import ParseLib.Parallel.Core
import ParseLib.Parallel.Derived

digit  :: Parser Char Char
digit  =  satisfy isDigit

newdigit :: Parser Char Int
newdigit = read . (:[]) <$> digit

natural :: Parser Char Int
natural = foldl (\ a b -> a * 10 + b) 0 <$> many1 newdigit

integer :: Parser Char Int
integer = (const negate <$> (symbol '-')) `option` id  <*>  natural 

identifier :: Parser Char String
identifier = (:) <$> satisfy isAlpha <*> greedy (satisfy isAlphaNum)

parenthesised :: Parser Char a -> Parser Char a
parenthesised p = pack (symbol '(') p (symbol ')')

bracketed :: Parser Char a -> Parser Char a
bracketed p = pack (symbol '[') p (symbol ']')

braced :: Parser Char a -> Parser Char a
braced p = pack (symbol '{') p (symbol '}')

commaList :: Parser Char a -> Parser Char [a]
commaList p = listOf p (symbol ',')

semiList :: Parser Char a -> Parser Char [a]
semiList p = listOf p (symbol ';')

