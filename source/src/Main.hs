
module Main where

    import Prelude hiding (writeFile, empty)

    import System.IO hiding (writeFile)
    import System.FilePath.Posix

    import ParseLib.Abstract hiding (empty)

    import Data.Map hiding (map, fold, foldr)
    import Data.Aeson
    import Data.Aeson.Encode.Pretty
    import Data.ByteString.Lazy (writeFile)

    import Model
    import Lexer
    import Parser
    import Algebra

    runScanner :: String -> IO()
    runScanner path = do 
        -- generic information from the path
        let directory = dropFileName path 
        let name = takeBaseName path
        let output = (directory </> name) ++ ".json"

        -- loading the file
        file <- readFile path
        putStrLn "File loaded"

        -- generating the tokens
        let tokens = (postProcessTokens . alexScanTokens) file 
        putStrLn "Generated tokens"

        -- generating the signatures
        let signatures = case parse findSnippets tokens of
                []          -> []
                ((s, a):_)  -> s
        putStrLn "Generated signatures"

        -- cleaing up the signatures
        let noCommentTags = map (fold removeCommentTags) signatures 
        let noEmptyComments = map (fold removeEmptyComments) noCommentTags
        let appendFilename = map (fold (addFileName name)) noEmptyComments
        putStrLn "Folded into snippets"

        -- generating the snippets
        let snippets = map (fold toSnippet) appendFilename 
        let dictionary = constructDictionary snippets 

        writeFile output (encodePretty dictionary)
        putStrLn ("Wrote output to: " ++ output)

    constructDictionary :: [Snippet] -> Map String Snippet 
    constructDictionary ss = foldr f b ss 
        where
            f :: Snippet -> Map String Snippet -> Map String Snippet 
            f s = insert (head $ prefix s) s 

            b :: Map String Snippet
            b = empty 

    -- Adds in an additional comment token in front of each function token
    postProcessTokens :: [Token] -> [Token]
    postProcessTokens ts = foldr f b ts
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