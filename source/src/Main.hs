
module Main where

    import Prelude hiding (writeFile, empty)

    import System.IO hiding (writeFile)
    import System.Environment
    import System.FilePath.Posix

    import ParseLib.Abstract hiding (empty)

    import Data.Maybe (fromMaybe)
    import Data.Map hiding (map, fold, foldr, foldl)
    import Data.Aeson hiding (Options, defaultOptions)
    import Data.Aeson.Encode.Pretty
    import Data.ByteString.Lazy (writeFile)

    import Options
    import Model
    import Lexer
    import Parser
    import Algebra

    main :: IO()
    main = do 
        -- load in arguments
        args        <- getArgs
        (opts, _)   <- compilerOpts args

        -- load in file
        lf <- loadFile (optFile opts)

        -- determine the name of the output file
        let filename = case optPrefix opts of 
                (Just p) -> p ++ ""
                Nothing  -> name lf ++ ""

        -- determine the context of the function
        let source = case optAddFileName opts of 
                False -> ""
                True  -> filename ++ "."

        -- process the file
        let ss  = processFile lf                -- turn file into signatures
        let ss' = postProcessSignatures ss      -- clean up the signatures
        let ds  = processSignatures source ss'  -- turn the signatures into snippets

        let output = ((directory lf) </> filename) ++ ".code-snippets"
        writeFile output (encodePretty ds)

    loadFile :: String -> IO LoadedFile
    loadFile path = do 
        -- generic information from the path
        let directory = dropFileName path 
        let name = takeBaseName path

        -- loading the file
        file <- readFile path

        -- return the file
        return LoadedFile { 
              directory = directory
            , name = name
            , content = file 
            , path = path
            }

    -- processes the file, turning it into signatures
    processFile :: LoadedFile -> [Signature]
    processFile lf = signatures 
        where
            -- runs the tokenizer over the file
            tokens :: [Token]
            tokens = (postProcessTokens . alexScanTokens) (content lf) 

            -- runs the parser over the tokens
            signatures :: [Signature]
            signatures = case parse findSnippets tokens of
                []          -> []
                ((s, a):_)  -> s

    -- post processes the signatures, removing inconsistencies
    postProcessSignatures :: [Signature] -> [Signature]
    postProcessSignatures signatures = noEmptyComments
        where
            -- remove '--', '#' and initial whitespace
            noCommentTags = map (fold removeCommentTags) signatures 

            -- remove empty comments
            noEmptyComments = map (fold removeEmptyComments) noCommentTags

    -- processes the signatures into a dictionary of snippets
    processSignatures :: Source ->  [Signature] -> Map String Snippet
    processSignatures source signatures = constructDictionary $ map (fold (toSnippet source)) signatures
            

    -- runScanner :: String -> IO()
    -- runScanner path = do 
    --     -- generic information from the path
    --     let directory = dropFileName path 
    --     let name = takeBaseName path
    --     let output = (directory </> name) ++ ".json"

    --     -- loading the file
    --     file <- readFile path
    --     putStrLn "File loaded"

    --     -- generating the tokens
    --     let tokens = (postProcessTokens . alexScanTokens) file 
    --     putStrLn "Generated tokens"

    --     -- generating the signatures
    --     let signatures = case parse findSnippets tokens of
    --             []          -> []
    --             ((s, a):_)  -> s
    --     putStrLn "Generated signatures"

    --     -- cleaing up the signatures
    --     let noCommentTags = map (fold removeCommentTags) signatures 
    --     let noEmptyComments = map (fold removeEmptyComments) noCommentTags
    --     let appendFilename = map (fold (addFileName name)) noEmptyComments
    --     putStrLn "Folded into snippets"

    --     -- generating the snippets
    --     let snippets = map (fold toSnippet) appendFilename 
    --     let dictionary = constructDictionary snippets 

    --     writeFile output (encodePretty dictionary)
    --     putStrLn ("Wrote output to: " ++ output)

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