module ParseLib.Abstract.Core
  (
    -- * The type of parsers
    Parser(),
    -- * Elementary parsers
    anySymbol,
    satisfy,
    empty, failp,
    succeed, pure,
    -- * Parser combinators
    (<|>),
    (<<|>),
    (<*>),
    (<$>),
    (>>=),
    -- * Lookahead
    look,
    -- * Running parsers
    parse
  )
  where

import Data.Char
import Data.Traversable
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified ParseLib.Simple.Core as SP

-- | An input string is mapped to a list of successful parses.
-- For each succesful parse, we return the result of type 'r',
-- and the remaining input string. The input must be a list of
-- symbols.
newtype Parser s r  =  Parser { runParser :: [s] -> [(r,[s])] }

instance Functor (Parser s) where
  fmap f p  =  Parser (f SP.<$> runParser p)

instance Applicative (Parser s) where
  pure x    =  Parser (SP.succeed x)
  p <*> q   =  Parser (runParser p SP.<*> runParser q)

instance Alternative (Parser s) where
  empty     =  Parser SP.empty
  p <|> q   =  Parser (runParser p SP.<|> runParser q)

infixr 3 <<|>

-- | Biased choice. If the left hand side parser succeeds,
-- the right hand side is not considered. Use with care!
(<<|>) :: Parser s a -> Parser s a -> Parser s a
p <<|> q  =  Parser (runParser p SP.<<|> runParser q)

instance Monad (Parser s) where
  return    =  pure
  p >>= f   =  Parser (runParser p SP.>>= (runParser . f))

instance MonadPlus (Parser s) where
  mzero     =  empty
  mplus     =  (<|>)

-- | Parses any single symbol.
anySymbol :: Parser s s
anySymbol = Parser (SP.anySymbol)

-- | Takes a predicate and returns a parser that parses a
-- single symbol satisfying that predicate.
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser (SP.satisfy p)

-- | Parser that always succeeds, i.e., for epsilon.
succeed :: a -> Parser s a
succeed = pure

-- | Same as 'empty'; provided for compatibility with the lecture notes.
failp :: Parser s a
failp = empty

-- | Returns the rest of the input without consuming anything.
look :: Parser s [s]
look = Parser (\ xs -> [(xs, xs)])

-- | Runs a parser on a given string.
parse :: Parser s a -> [s] -> [(a,[s])]
parse = runParser
