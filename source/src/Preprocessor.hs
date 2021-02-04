
module Preprocessor where

    import Prelude hiding ((<$), (<*>), (<*))

    import Data.List
    import Data.Maybe 

    import ParseLib.Abstract

    import Model 

    transformMetaSignatures :: String -> Parser Token [Token] 
    transformMetaSignatures c = concat <$> greedy (choice
            [
                  (transformMetaSignature c)
                , (\s -> [s]) <$> anySymbol
            ] 
        )

    transformMetaSignature :: String -> Parser Token [Token]
    transformMetaSignature c = do 
        -- find the pieces
        id <- satisfy isIdentifier
        eq <- symbol TokenEqual
        fn <- symbol TokenFunction
        ps <- findParameters

        let id' = case id of (TokenIdentifier s) -> s 
        let ps' = drop 1 ps 

        -- re-configure them
        let pre = [ TokenFunction, TokenIdentifier c, TokenColon, TokenIdentifier id', TokenOpen]
        let prs = intersperse TokenComma ps' 
        let pos = [ TokenClose ]

        -- put them back in
        return $ concat [pre, prs, pos]

    -- parses all the parameters
    findParameters :: Parser Token [Token]
    findParameters = pack (symbol TokenOpen) (listOf (satisfy isIdentifier) (symbol TokenComma)) (symbol TokenClose)
