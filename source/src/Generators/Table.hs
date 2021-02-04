
module Generators.Table where 


    import Prelude hiding ((<$), (<*>), (<*), (*>))

    import System.Environment
    import System.FilePath.Posix

    import Data.List
    import Data.Maybe (fromMaybe)
    import ParseLib.Abstract

    import Configuration
    import Lexer
    import Model (Token(..), Snippet(..))

    import Generators.Common 

    --- | The signature format that this generator can deal with
    data Signature = Signature {
          sigName :: String
        , sigParams :: [String]
        , sigComments :: [String]
        , sigClass :: String
    } deriving (Show, Eq)

    --- | Generates snippets of function signatures inside 'file'.
    generate :: Configuration -> String -> IO [Snippet]
    generate config file = do 
        tokens      <- tokenize config file
        signatures  <- signaturize config tokens 
        return $ snippetize config signatures
    
    --- | Transforms 'file' into a series of tokens for further processing.
    tokenize :: Configuration -> String -> IO [Token]
    tokenize config content = do 
        -- retrieve relevant information
        let debug   = conDebug      config     
        let direc   = conDirectory  config
        let filen   = conFile       config

        -- get the tokens
        let tokens = alexScanTokens (content)

        case debug of 
            True -> do  putStrLn ("Writing out pre-token stream...")
                        writeFile (direc </> filen ++ ".pretoken") (show tokens)
            False ->    return ()

        -- post-process them
        let tokens' = postProcessTokens tokens 

        case debug of 
            True -> do  putStrLn ("Writing out post-token stream...")
                        writeFile (direc </> filen ++ ".posttoken") (show tokens')
            False ->    return ()

        -- make it our return value
        return tokens'

    --- | Post processes the tokens by adding a 
    -- comment token before each function tokens.
    postProcessTokens :: [Token] -> [Token]
    postProcessTokens tokens = addCommentTokens tokens 

    --- | Parses the tokens into signatures.
    signaturize :: Configuration -> [Token] -> IO [Signature]
    signaturize config tokens = do
        -- retrieve relevant information
        let debug   = conDebug      config     
        let direc   = conDirectory  config
        let filen   = conFile       config

        -- parse the signtures
        let signatures = case parse (parseSignatures parseSignature) tokens of
                []          -> []
                ((s, a):_)  -> s

        case debug of 
                True -> do  putStrLn ("Writing out signatures stream...")
                            writeFile (direc </> filen ++ ".presigs") (show signatures)
                False ->    return () 

        -- post process the signature stream
        let signatures' = postProcessSignatures signatures

        case debug of 
                True -> do  putStrLn ("Writing out signatures stream...")
                            writeFile (direc </> filen ++ ".postsigs") (show signatures')
                False ->    return () 

        -- make it our return value
        return signatures  

    --- | Post processes the signatures by cleaning up the comments.
    postProcessSignatures :: [Signature] -> [Signature]
    postProcessSignatures signatures = map postProcessSignature signatures
        where
            postProcessSignature :: Signature -> Signature
            postProcessSignature sig = sig { 
                    sigComments = (removeEmptyStrings . removeCommentTags) (sigComments sig) 
                }

    --- | Transforms the signatures into snippets.
    snippetize :: Configuration -> [Signature] -> [Snippet]
    snippetize config signatures = map toSnippet signatures
        where
            -- retrieve information
            c' = case conAppend config of 
                False -> Nothing
                True -> Just $ conClass config

            --- | Turns a single signature into the corresponding snippet.
            toSnippet :: Signature -> Snippet
            toSnippet (Signature n ps cs c) = Snippet prefix body description  
                where
                    -- use the global class if available
                    cl = fromMaybe c c'

                    prefix :: [String]
                    prefix = [cl ++ "." ++ n]

                    body :: [String]
                    body = [cl ++ "." ++ n ++ "(" ++ ps' ++ ")"]
                        where
                            ps' :: String
                            ps' = intercalate ", " (map (uncurry format) (zip [1..] ps )) 

                            -- formats the argument
                            format :: Int -> String -> String 
                            format i s = "${" ++ (show i) ++ ":" ++ s ++ "}"
                    
                    description :: String 
                    description = concat $ intersperse ("\r\n") cs

    --- | Attempts to parse a signature from the token stream.
    parseSignature :: Parser Token Signature 
    parseSignature = do 
        -- find the relevant information
        cs <- findComments
        _  <- symbol TokenFunction
        ct <- satisfy isIdentifier
        _  <- symbol TokenDot 
        id <- satisfy isIdentifier
        ps <- findParameters

        -- transform the information
        let name = case id of (TokenIdentifier s) -> s 
        let comments = map (\(TokenComment s) -> s) cs 
        let params = map (\(TokenIdentifier s) -> s) ps 
        let cid = case ct of (TokenIdentifier s) -> s

        -- store it as a snippet
        return $ Signature {
              sigComments = comments
            , sigParams = params
            , sigName = name 
            , sigClass = cid
        }
