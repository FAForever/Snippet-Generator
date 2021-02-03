
module Algebra where

    import Model

    import Data.Char
    import Data.List

    -- a very basic algebra
    type Algebra r = (
            Comments -> Params -> Name -> r
        )

    -- a very basic fold
    fold :: Algebra r -> Signature -> r 
    fold (snippet) (FunctionSignature cs ps n) = snippet cs ps n   

    -- removes comment tags such as '-' or '#' at the start of the string
    removeCommentTags :: Algebra Signature
    removeCommentTags cs ps n = FunctionSignature cs' ps n 
        where
            cs' :: Comments 
            cs' = map (dropWhile (\c -> isCommentTag c || isSpace c)) cs 

            isCommentTag :: Char -> Bool 
            isCommentTag '-' = True 
            isCommentTag '#' = True 
            isCommentTag _   = False 

    -- filters out empty comments
    removeEmptyComments :: Algebra Signature
    removeEmptyComments cs ps n = FunctionSignature cs' ps n 
        where
            cs' :: Comments 
            cs' = filter (\s -> not (s == "")) cs

    -- adds in the file name to the name of the function
    addFileName :: Name -> Algebra Signature
    addFileName f cs ps n = FunctionSignature cs ps (f ++ "." ++ n) 

    -- type Snippets = Map String Snippet
    -- data Snippet = Snippet {
    --       prefix :: [String]
    --     , body :: [String]
    --     , description :: String
    -- }

    toSnippet :: Algebra Snippet 
    toSnippet cs ps n = Snippet prefix body description
        where
            prefix :: [String]
            prefix = [n]

            body :: [String]
            body = [n ++ "(" ++ ps' ++ ")"]
                where
                    ps' :: String
                    ps' = intercalate ", " (map (uncurry format) (zip [1..] ps )) 

                    -- formats the argument
                    format :: Int -> String -> String 
                    format i s = "${" ++ (show i) ++ ":" ++ s ++ "}"
            
            description :: String 
            description = concat $ intersperse ("\r\n") cs

