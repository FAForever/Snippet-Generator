
module Algebra where

    import Model

    import Data.Char
    import Data.List

    -- a very basic algebra
    type Algebra r = (
            -- FunctionSignature
            Comments -> Params -> Name -> r,

            -- MetatableSignature
            Comments -> Params -> Name -> String -> r
        )

    -- a very basic fold
    fold :: Algebra r -> Signature -> r 
    fold (func, meta) (FunctionSignature cs ps n)       = func cs ps n  
    fold (func, meta) (MetatableSignature cs ps n c)    = meta cs ps n c

    isCommentTag :: Char -> Bool 
    isCommentTag '-' = True 
    isCommentTag '#' = True 
    isCommentTag _   = False 

    -- removes comment tags such as '-' or '#' at the start of the string
    removeCommentTags :: Algebra Signature
    removeCommentTags = (functionSignature, metaSignature)
        where
            functionSignature :: Comments -> Params -> Name -> Signature
            functionSignature cs ps n = FunctionSignature (removeCommentTags' cs) ps n 

            metaSignature :: Comments -> Params -> Name -> String -> Signature
            metaSignature cs ps n c = MetatableSignature (removeCommentTags' cs) ps n c 



    -- filters out empty comments
    removeEmptyComments :: Algebra Signature
    removeEmptyComments = (functionSignature, metaSignature)
        where
            functionSignature :: Comments -> Params -> Name -> Signature
            functionSignature cs ps n = FunctionSignature (removeEmptyComments' cs) ps n 

            metaSignature :: Comments -> Params -> Name -> String -> Signature
            metaSignature cs ps n c = MetatableSignature (removeEmptyComments' cs) ps n c  

            removeEmptyComments' :: Comments -> Comments 
            removeEmptyComments' cs = filter (\s -> not (s == "")) cs

    -- type Snippets = Map String Snippet
    -- data Snippet = Snippet {
    --       identifier :: [String]
    --     , body :: [String]
    --     , description :: String
    -- }

    toSnippet :: Source -> Algebra Snippet 
    toSnippet s = (functionSignature, metaSignature)
        where


            metaSignature :: Comments -> Params -> Name -> String -> Snippet
            metaSignature cs ps n c = Snippet prefix body description  
                where
                    prefix :: [String]
                    prefix = [c ++ ":" ++ n]

                    body :: [String]
                    body = ["${1:" ++ c ++ "}" ++ ":" ++ n ++ "(" ++ ps' ++ ")"]
                        where
                            ps' :: String
                            ps' = intercalate ", " (map (uncurry format) (zip [2..] ps )) 

                            -- formats the argument
                            format :: Int -> String -> String 
                            format i s = "${" ++ (show i) ++ ":" ++ s ++ "}"
                    
                    description :: String 
                    description = concat $ intersperse ("\r\n") cs


