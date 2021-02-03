
module Parser where

import Prelude hiding ((<$), (<*>), (<*))
import Data.Maybe 
import ParseLib.Abstract

import Model 

findSnippets :: Parser Token [Signature] 
findSnippets = catMaybes <$> greedy (choice
        [
            Just    <$> findSnippet <|>
            Nothing <$ anySymbol
        ] 
    )

findSnippet :: Parser Token Signature
findSnippet = do 
    -- find the relevant information
    cs <- findComments
    _  <- symbol TokenFunction
    id <- satisfy isIdentifier
    ps <- findParameters

    -- transform the information
    let name = case id of (TokenIdentifier s) -> s 
    let comments = map (\(TokenComment s) -> s) cs 
    let params = map (\(TokenIdentifier s) -> s) ps 

    -- store it as a snippet
    return $ FunctionSignature comments params name

findComments :: Parser Token [Token] 
findComments = greedy (satisfy isComment)

findParameters :: Parser Token [Token]
findParameters = pack (symbol TokenOpen) (listOf (satisfy isIdentifier) (symbol TokenComma)) (symbol TokenClose)

-- removes all tokens until we find a comment token
-- after post processing we are ensured that a function starts with an (possibly empty) comment token
riddleThroughTrash :: Parser Token ()
riddleThroughTrash = () <$ greedy (satisfy (not . isComment)) 

-- removes all comment tokens
riddleThroughComments :: Parser Token ()
riddleThroughComments = () <$ greedy (satisfy isComment) 