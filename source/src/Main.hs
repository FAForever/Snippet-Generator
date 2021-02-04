
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