module ParseLib.Simple.Core
  (
    -- * The type of parsers
    Parser,
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

import Prelude hiding ((>>=), (<*>), (<$>), pure)

-- | An input string is mapped to a list of successful parses.
-- For each succesful parse, we return the result of type 'r',
-- and the remaining input string. The input must be a list of
-- symbols.
type Parser s r  =  [s] -> [(r,[s])]

-- | Parses any single symbol.
anySymbol :: Parser s s
anySymbol (x:xs)  =  [(x,xs)]
anySymbol []      =  []

-- | Takes a predicate and returns a parser that parses a
-- single symbol satisfying that predicate.
satisfy  ::  (s -> Bool) -> Parser s s
satisfy p (x:xs) | p x       =  [(x,xs)]
satisfy _ _                  =  []

-- | Parser for the empty language, i.e., parser that always fails.
empty :: Parser s a
empty xs  =  []

-- | Same as 'empty'; provided for compatibility with the lecture notes.
failp :: Parser s a
failp = empty

-- | Parser that always succeeds, i.e., for epsilon.
succeed :: a -> Parser s a
succeed r xs = [(r,xs)]

-- | Same as 'succeed'; provided for compatiblity with the applicative
-- interface.
pure :: a -> Parser s a
pure = succeed

infixl 4  <$>, <*>
infixr 3  <|>, <<|>
infixl 1  >>=

-- | Choice between two parsers with the same result type.
(<|>) :: Parser s a -> Parser s a -> Parser s a
(p <|> q) xs  =  p xs ++ q xs

-- | Biased choice. If the left hand side parser succeeds,
-- the right hand side is not considered. Use with care!
(p <<|> q) xs  =  let r = p xs in if null r then q xs else r

-- | Sequence of two parsers.
(<*>) :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) xs  =  [(f x,zs)
                 |(f  ,ys) <- p xs
                 ,(  x,zs) <- q ys
                 ]

-- | Map a function over the results of a parser. The '<$>' combinator
-- can also be defined in terms of 'succeed' and '<*>':
--
-- > f <$> p  =  succeed f <*> p
--
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs  =  [(f y,ys)
                 |(  y,ys) <- p xs
                 ]

-- | Monadic bind. Do not use this combinator unless absolutely
-- required. Most sequencing can be done with '<*>'.
(>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
(p >>= f) xs  =  [(z  ,zs)
                 |(y  ,ys) <- p xs
                 ,(z  ,zs) <- f y ys
                 ]

-- | Returns the rest of the input without consuming anything.
look :: Parser s [s]
look xs = [(xs, xs)]

-- | For compatibility with the "newtype" version of the library:
-- runs a parser on a given string.
parse :: Parser s a -> [s] -> [(a, [s])]
parse = id
