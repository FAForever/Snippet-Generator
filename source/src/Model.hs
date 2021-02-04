
module Model where

    import GHC.Generics

    import Data.Aeson
    import Data.Map

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

    -- data LoadedFile = LoadedFile {
    --       directory :: String
    --     , name :: String
    --     , path :: String
    --     , content :: String
    -- }

    -- type Name = String
    -- type Source = String
    -- type Params = [String]
    -- type Comments = [String]
    -- data Signature 
    --     = FunctionSignature {
    --               sigComments:: Comments
    --             , sigParams :: Params
    --             , sigName :: Name 
    --         }
    --     | MetatableSignature {
    --               metaComments :: Comments
    --             , metaParams :: Params
    --             , metaName :: Name
    --             , metaContext :: String
    --         }
    --     deriving (Generic, Show, Eq)

    type Snippets = Map String Snippet
    data Snippet = Snippet {
          prefix :: [String]
        , body :: [String]
        , description :: String
    } deriving ( Generic, Show, Eq )

    instance ToJSON Snippet where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Snippet