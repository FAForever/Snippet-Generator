
module Parser where

import Prelude hiding ((<$), (<*>), (<*), (*>))
import Data.Maybe 
import ParseLib.Abstract

import Model 

findSignatures :: Parser Token [Signature] 
findSignatures = catMaybes <$> greedy (choice
        [
            Just    <$> findSignature <|>
            Nothing <$ anySymbol
        ] 
    )

findSignature :: Parser Token Signature
findSignature = findFunctionSignature <|> findMetaSignature

findFunctionSignature :: Parser Token Signature 
findFunctionSignature = do 
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
    return $ FunctionSignature {
          sigComments = comments
        , sigParams = params
        , sigName = name 
    }

-- function GetMarkers()
-- function UnitWeapon:ChangeDamage(value)

-- unit:OnPrecreate
--     OnPreCreate = function(self)
-- function <class>:OnPreCreate

findMetaSignature :: Parser Token Signature 
findMetaSignature = do 
    -- find the relevant information
    cs <- findComments
    _  <- symbol TokenFunction
    ct <- satisfy isIdentifier
    _  <- symbol TokenColon 
    id <- satisfy isIdentifier
    ps <- findParameters

    -- transform the information
    let name = case id of (TokenIdentifier s) -> s 
    let comments = map (\(TokenComment s) -> s) cs 
    let params = map (\(TokenIdentifier s) -> s) ps 
    let context = case ct of (TokenIdentifier s) -> s 

    -- store it as a snippet
    return $ MetatableSignature {
          metaComments = comments
        , metaParams = params
        , metaName = name 
        , metaContext = context
    }

findComments :: Parser Token [Token] 
findComments = greedy (satisfy isComment)

findParameters :: Parser Token [Token]
findParameters = pack (symbol TokenOpen) (listOf' (satisfy isIdentifier) (symbol TokenComma)) (symbol TokenClose)
    where
        listOf' :: Parser s a -> Parser s b -> Parser s [a]
        listOf' p s = ((:) <$> p <*> many (s *> p)) <|> (return [])


-- removes all tokens until we find a comment token
-- after post processing we are ensured that a function starts with an (possibly empty) comment token
riddleThroughTrash :: Parser Token ()
riddleThroughTrash = () <$ greedy (satisfy (not . isComment)) 

-- removes all comment tokens
riddleThroughComments :: Parser Token ()
riddleThroughComments = () <$ greedy (satisfy isComment) 