
module Model where

    import GHC.Generics

    import Data.Aeson
    import Data.Map

    data Token  = TokenFunction
                | TokenComment String 
                | TokenOpen
                | TokenClose
                | TokenComma
                | TokenColon
                | TokenIdentifier String 
                | TokenEmpty 
                deriving (Generic, Show, Eq)

    -- checks whether a token is a comment
    isComment :: Token -> Bool 
    isComment (TokenComment _)  = True 
    isComment _                 = False

    -- checks whether a token is an identifier
    isIdentifier :: Token -> Bool 
    isIdentifier (TokenIdentifier _) = True 
    isIdentifier _                   = False

    type Name = String
    type Params = [String]
    type Comments = [String]
    data Signature = FunctionSignature Comments Params Name 
                   | MetatableSignature Comments Params Name 
                   deriving (Generic, Show, Eq)

    type Snippets = Map String Snippet
    data Snippet = Snippet {
          prefix :: [String]
        , body :: [String]
        , description :: String
    } deriving ( Generic, Show, Eq )

    instance ToJSON Signature where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Signature

    instance ToJSON Snippet where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Snippet