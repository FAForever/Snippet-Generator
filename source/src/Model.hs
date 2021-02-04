
module Model where

    import GHC.Generics
    import Data.Aeson

    --- | A simple token that takes out the 
    -- important bits and pieces from a lua file.
    data Token  = TokenFunction
                | TokenComment String 
                | TokenOpen
                | TokenClose
                | TokenComma
                | TokenDot
                | TokenColon
                | TokenEqual
                | TokenIdentifier String 
                | TokenEmpty 
                deriving (Generic, Show, Eq)

    --- | A snippet that represents a function with a prefix 
    -- (trigger), body (snippet itself) and a description.
    data Snippet = Snippet {
          prefix :: [String]
        , body :: [String]
        , description :: String
    } deriving ( Generic, Show, Eq )

    -- allows us to easily encode it
    instance FromJSON Snippet
    instance ToJSON Snippet where
        toEncoding = genericToEncoding defaultOptions
