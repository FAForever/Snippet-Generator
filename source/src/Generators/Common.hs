
module Generators.Common where 

    import Prelude hiding ((<$), (<*>), (<*), (*>))
    import ParseLib.Abstract hiding (empty)

    import Debug.Trace

    import Data.Char
    import Data.Maybe
    import Data.Map hiding (foldr, map, filter)

    import Model

    -- TOKEN FUNCTIONALITY --

    --- | Guarantees the assumption that there is always an 
    -- (empty) comment block before a function token.
    addCommentTokens :: [Token] -> [Token]
    addCommentTokens ts = foldr f b ts
        where
            f :: Token -> [Token] -> [Token]
            f t acc 
                | isFunction t = (TokenComment "") : t : acc 
                | otherwise    = t : acc 

            b :: [Token]
            b = [] 

            -- checks whether a token is a function token
            isFunction :: Token -> Bool 
            isFunction TokenFunction = True 
            isFunction _             = False

    -- checks whether a token is a comment
    isComment :: Token -> Bool 
    isComment (TokenComment _)  = True 
    isComment _                 = False

    -- checks whether a token is an identifier
    isIdentifier :: Token -> Bool 
    isIdentifier (TokenIdentifier _) = True 
    isIdentifier _                   = False

    -- SIGNATURE FUNCTIONALITY --
    
    -- | Removes empty strings from the list of strings 
    removeEmptyStrings :: [String] -> [String] 
    removeEmptyStrings cs = filter (\s -> not (s == "")) cs

    --- | Removes the comment tags at the start and end of the strings
    removeCommentTags :: [String] -> [String] 
    removeCommentTags cs = map reverse rs'
        where
            cs' = map (dropWhile (\c -> isCommentTag c || isSpace c)) cs
            rs = map reverse cs'
            rs' = map (dropWhile (\c -> isCommentTag c || isSpace c)) rs

    --- | Checks whether the character is considered a comment tag
    isCommentTag :: Char -> Bool 
    isCommentTag '-' = True 
    isCommentTag '#' = True 
    isCommentTag '[' = True
    isCommentTag ']'  = True
    isCommentTag _   = False 


    findComments :: Parser Token [Token] 
    findComments = greedy (satisfy isComment)

    findParameters :: Parser Token [Token]
    findParameters = pack (symbol TokenOpen) (listOf' (satisfy isIdentifier) (symbol TokenComma)) (symbol TokenClose)
        where
            listOf' :: Parser s a -> Parser s b -> Parser s [a]
            listOf' p s = ((:) <$> p <*> many (s *> p)) <|> (return [])

    --- | Parses all signatures from the token stream.
    parseSignatures :: Parser Token a -> Parser Token [a] 
    parseSignatures sig = catMaybes <$> greedy (Just <$> sig <|> Nothing <$ anySymbol)

    -- SNIPPET FUNCTIONALITY --

    --- | Constructs a dictionary from the list of 'snippets'.
    toDictionary :: [Snippet] -> Map String Snippet 
    toDictionary snippets = foldr f b snippets 
        where
            f :: Snippet -> Map String Snippet -> Map String Snippet 
            f s = insert (head $ prefix s) s 

            b :: Map String Snippet
            b = empty 
