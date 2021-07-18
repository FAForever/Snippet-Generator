module ParseLib.Abstract.Derived
  (
    module ParseLib.Abstract.Core,
    -- * Derived combinators
    (<$),
    (<*),
    (*>),
    epsilon,
    symbol,
    token,
    pack,
    sequence,
    choice,
    -- * EBNF parser combinators 
    option,
    optional,
    many,
    some, many1,
    listOf,
    -- * Chain expression combinators
    chainr,
    chainl,
    -- * Greedy parsers
    greedy,
    greedy1,
    -- * End of input
    eof
  )
  where

import Prelude hiding ((>>=), (<$), (<*), (*>), sequence)
import ParseLib.Abstract.Core

infixl 4 <$
infixl 4 <*
infixl 4 *>

-- | Variant of '<$>' that ignores the result of the parser.
--
-- > f <$ p = const f <$> p
--
(<$) :: b -> Parser s a -> Parser s b
f <$ p = const f <$> p

-- | Variant of '<*>' that ignores the result of the right
-- argument.
--
-- > f <* p = const <$> p <*> q
--
(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = const <$> p <*> q

-- | Variant of '*>' that ignores the result of the left
-- argument.
--
-- > f *> p = flip const <$> p <*> q
--
(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = flip const <$> p <*> q

-- | Parser for epsilon that does return '()'.
epsilon :: Parser s ()
epsilon = succeed ()

-- | Parses a specific given symbol.
symbol :: Eq s  => s -> Parser s s
symbol x = satisfy (==x)

-- | Parses a specific given sequence of symbols.
token :: Eq s => [s] -> Parser s [s]
token []     = succeed []
token (x:xs) = (:) <$> symbol x <*> token xs

-- | Takes three parsers: a delimiter, the parser for the
-- content, and another delimiter. Constructs a sequence of
-- the three, but returns only the result of the enclosed
-- parser.
pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack p r q  =  p *> r <* q

-- | Takes a list of parsers and combines them in
-- sequence, returning a list of results.
sequence :: [Parser s a] -> Parser s [a]
sequence []      =  succeed []
sequence (p:ps)  =  (:) <$> p <*> sequence ps

-- | Takes a list of parsers and combines them using
-- choice.
choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) empty

-- | Parses an optional element. Takes the default value
-- as its second argument.
option :: Parser s a -> a -> Parser s a
option p d = p <|> succeed d

-- | Variant of 'option' that returns a 'Maybe',
-- provided for compatibility with the applicative interface.
optional :: Parser s a -> Parser s (Maybe a)
optional p = option (Just <$> p) Nothing

-- | Parses many, i.e., zero or more, occurrences of
-- a given parser.
many :: Parser s a  -> Parser s [a]
many p  =  (:) <$> p <*> many p <|> succeed []

-- | Parser some, i.e., one or more, occurrences of
-- a given parser.
some :: Parser s a -> Parser s [a]
some p = (:) <$> p <*> many p

-- | Same as 'some'. Provided for compatibility with
-- the lecture notes.
many1 :: Parser s a -> Parser s [a]
many1 = some

-- | Takes a parser @p@ and a separator parser @s@. Parses
-- a sequence of @p@s that is separated by @s@s.
listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = (:) <$> p <*> many (s *> p)

-- | Takes a parser @pe@ and an operator parser @po@. Parses
-- a sequence of @pe@s separated by @po@s. The results are
-- combined using the operator associated with @po@ in a
-- right-associative way.
chainr  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe po  =  h <$> many (j <$> pe <*> po) <*> pe
  where j x op  =  (x `op`)
        h fs x  =  foldr ($) x fs

-- | Takes a parser @pe@ and an operator parser @po@. Parses
-- a sequence of @pe@s separated by @po@s. The results are
-- combined using the operator associated with @po@ in a
-- left-associative way.
chainl  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl pe po  =  h <$> pe <*> many (j <$> po <*> pe)
  where j op x  =  (`op` x)
        h x fs  =  foldl (flip ($)) x fs

-- | Greedy variant of 'many'.
greedy :: Parser s b -> Parser s [b]
greedy p = (:) <$> p <*> greedy p <<|> succeed []

-- | Greedy variant of 'many1'.
greedy1 :: Parser s b -> Parser s [b]
greedy1 p = (:) <$> p <*> greedy p

-- | Succeeds only on the end of the input.
eof :: Parser s ()
eof = look >>= \ xs -> if null xs then succeed () else failp

