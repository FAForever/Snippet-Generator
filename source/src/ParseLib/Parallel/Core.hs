{-# LANGUAGE RankNTypes #-}
module ParseLib.Parallel.Core
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

-- | The parser is a CPS version of Parser'
newtype Parser s r  =  Parser (forall a. (r -> Parser' s a) -> Parser' s a)

runParser :: Parser s r -> (r -> Parser' s a) -> Parser' s a
runParser (Parser p) = p

data Parser' s r = SymbolBind (s -> Parser' s r)
                 | Fail
                 | ReturnPlus r (Parser' s r)
                 | LookBind ([s] -> Parser' s r)

instance Functor (Parser s) where
  fmap f p  =  p >>= \ r -> return (f r)

instance Applicative (Parser s) where
  pure x    =  return x
  p <*> q   =  p >>= \ f -> q >>= \ x -> return (f x)

instance Alternative (Parser s) where
  empty     =  mzero
  p <|> q   =  mplus p q

infixl 3 <<|>

-- | Biased choice. Not implemented by the parallel parser
-- combinators. Just maps to parallel choice.
(<<|>) :: Parser s a -> Parser s a -> Parser s a
(<<|>) = (<|>)

instance Monad (Parser s) where
  return x  =  Parser (\ k -> k x)
  p >>= f   =  Parser (\ k -> runParser p (\ x -> runParser (f x) k))

instance MonadPlus (Parser s) where
  mzero     =  Parser (\ k -> Fail)
  mplus p q =  Parser (\ k -> runParser p k +++ runParser q k)

(+++) :: Parser' s a -> Parser' s a -> Parser' s a
SymbolBind f   +++ SymbolBind g   = SymbolBind (\ x -> f x +++ g x)
Fail           +++ q              = q
p              +++ Fail           = p
ReturnPlus x p +++ q              = ReturnPlus x (p +++ q)
p              +++ ReturnPlus x q = ReturnPlus x (p +++ q)
LookBind f     +++ LookBind g     = LookBind (\ x -> f x +++ g x)
LookBind f     +++ q              = LookBind (\ x -> f x +++ q)
p              +++ LookBind g     = LookBind (\ x -> p +++ g x)

-- | Parses any single symbol.
anySymbol :: Parser s s
anySymbol = Parser (\ k -> SymbolBind k)

-- | Takes a predicate and returns a parser that parses a
-- single symbol satisfying that predicate.
satisfy :: (s -> Bool) -> Parser s s
satisfy p = anySymbol >>= \ x -> if p x then return x else mzero

-- | Parser that always succeeds, i.e., for epsilon.
succeed :: a -> Parser s a
succeed = pure

-- | Same as 'empty'; provided for compatibility with the lecture notes.
failp :: Parser s a
failp = empty

-- | Returns the rest of the input without consuming anything.
look :: Parser s [s]
look = Parser (\ k -> LookBind k)

-- | Runs a parser to a given string.
parse :: Parser s a -> [s] -> [(a,[s])]
parse p = parse' (runParser p (\ x -> ReturnPlus x Fail))

parse' :: Parser' s a -> [s] -> [(a,[s])]
parse' (SymbolBind f)    (x : xs)  =  parse' (f x) xs
parse' (ReturnPlus x p)  xs        =  (x,xs) : parse' p xs
parse' (LookBind f)      xs        =  parse' (f xs) xs
parse' _                 _         =  []
