
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

    import qualified Generators.Common as GenCommon
    import qualified Generators.Class as GenClass
    import qualified Generators.Function as GenFunction
    import qualified Generators.Metatable as GenMetatable
    import qualified Generators.Table as GenTable
    
    import Model
    import Configuration

    main :: IO()
    main = do 
        -- load in arguments
        args        <- getArgs
        (opts, _)   <- readArgs args
        config      <- interpretArgs opts

        -- load in file
        content <- readFile (conPath config)

        -- generate 'dem snippets
        snippets <- case (conType config) of 
            "class"         -> GenClass.generate config content
            "function"      -> GenFunction.generate config content
            "metatable"     -> GenMetatable.generate config content
            "table"         -> GenTable.generate config content

        -- write it all out!
        let dictionary = GenCommon.toDictionary snippets
        let output = ((conDirectory config) </> (conFile config)) ++ ".code-snippets"
        writeFile output (encodePretty dictionary)

    -- tokenize :: Options -> LoadedFile -> IO [Token]
    -- tokenize opts lf = do 
    --     -- retrieve relevant information
    --     let debug   = optDebug opts     
    --     let contx   = optClass opts 

    --     let direc   = directory lf
    --     let filen   = name lf

    --     -- get the tokens
    --     let tokens = alexScanTokens (content lf)

    --     case debug of 
    --         True -> do  putStrLn ("Writing out pre-token stream...")
    --                     writeFile (direc </> filen ++ ".pretoken") (encodePretty tokens)
    --         False ->    return ()

    --     -- post-process them
    --     let tokens' = case contx of 
    --             Nothing  -> postProcessTokens tokens 
    --             (Just c) -> postProcessTokens $ transformTokenStream c tokens

    --     case debug of 
    --         True -> do  putStrLn ("Writing out post-token stream...")
    --                     writeFile (direc </> filen ++ ".posttoken") (encodePretty tokens')
    --         False ->    return ()

    --     -- make it our return value
    --     return tokens'

    -- signaturize :: Options -> LoadedFile -> [Token] -> IO [Signature]
    -- signaturize opts lf ts = do 
    --     -- retrieve relevant information
    --     let debug   = optDebug opts 
    --     let contx   = optClass opts 

    --     let direc   = directory lf
    --     let filen   = name lf

    --     -- determine the name of the output file
    --     let filename = case optPrefix opts of 
    --             (Just p) -> p ++ ""
    --             Nothing  -> name lf ++ ""

    --     -- determine the context of the function
    --     let source = case optAddFileName opts of 
    --             False -> ""
    --             True  -> filename ++ "."

    --     -- process them into signatures
    --     let signatures = case parse findSignatures ts of
    --             []          -> []
    --             ((s, a):_)  -> s

    --     let count = length signatures
    --     case debug of 
    --         True -> do  putStrLn ("Writing out signatures (" ++ show count ++ ") stream...")
    --                     writeFile (direc </> filen ++ ".sigs") (encodePretty signatures)
    --         False ->    return () 

    --     -- make it our return value
    --     return signatures  

    -- snippetize :: Options -> LoadedFile -> [Signature] -> IO (Map String Snippet)
    -- snippetize opts lf ss = do    
    --     -- retrieve relevant information
    --     let debug   = optDebug opts 
    --     let contx   = optClass opts 

    --     let direc   = directory lf
    --     let filen    = name lf

    --     -- determine the name of the output file
    --     let filename = case optPrefix opts of 
    --             (Just p) -> p ++ ""
    --             Nothing  -> name lf ++ ""

    --     -- determine the context of the function
    --     let source = case optAddFileName opts of 
    --             False -> ""
    --             True  -> filename ++ "."

    --     -- turn them into the correct format
    --     let ds  = processSignatures source ss

    --     return ds


    -- loadFile :: String -> IO LoadedFile
    -- loadFile path = do 
    --     -- generic information from the path
    --     let directory = dropFileName path 
    --     let name = takeBaseName path

    --     -- loading the file
    --     file <- readFile path

    --     -- return the file
    --     return LoadedFile { 
    --           directory = directory
    --         , name = name
    --         , content = file 
    --         , path = path
    --         }

    -- tokenizeFile :: LoadedFile -> [Token]
    -- tokenizeFile lf = alexScanTokens (content lf)


    -- -- processes the file, turning it into signatures
    -- processFile :: LoadedFile -> [Signature]
    -- processFile lf = signatures 
    --     where
    --         -- runs the tokenizer over the file
    --         tokens :: [Token]
    --         tokens = (postProcessTokens . alexScanTokens) (content lf) 

    --         -- runs the parser over the tokens
    --         signatures :: [Signature]
    --         signatures = case parse findSignatures tokens of
    --             []          -> []
    --             ((s, a):_)  -> s

    -- -- post processes the signatures, removing inconsistencies
    -- postProcessSignatures :: [Signature] -> [Signature]
    -- postProcessSignatures signatures = noEmptyComments
    --     where
    --         -- remove '--', '#' and initial whitespace
    --         noCommentTags = map (fold removeCommentTags) signatures 

    --         -- remove empty comments
    --         noEmptyComments = map (fold removeEmptyComments) noCommentTags

    -- -- processes the signatures into a dictionary of snippets
    -- processSignatures :: Source ->  [Signature] -> Map String Snippet
    -- processSignatures source signatures = constructDictionary $ map (fold (toSnippet source)) signatures
    
    -- constructDictionary :: [Snippet] -> Map String Snippet 
    -- constructDictionary ss = foldr f b ss 
    --     where
    --         f :: Snippet -> Map String Snippet -> Map String Snippet 
    --         f s = insert (head $ prefix s) s 

    --         b :: Map String Snippet
    --         b = empty 

    -- transformTokenStream :: String -> [Token] -> [Token]
    -- transformTokenStream c ts = case parse (transformMetaSignatures c) ts of 
    --     []          -> []
    --     ((s, a):_)  -> s


    -- -- Adds in an additional comment token in front of each function token
    -- postProcessTokens :: [Token] -> [Token]
    -- postProcessTokens ts = foldr f b ts
    --     where
    --         f :: Token -> [Token] -> [Token]
    --         f t acc 
    --             | isFunction t = (TokenComment "") : t : acc 
    --             | otherwise    = t : acc 

    --         b :: [Token]
    --         b = [] 

    --         -- checks whether a token is a function token
    --         isFunction :: Token -> Bool 
    --         isFunction TokenFunction = True 
    --         isFunction _             = False