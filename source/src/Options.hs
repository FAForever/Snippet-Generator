
module Options (compilerOpts, optAddFileName, optFile, optPrefix) where

    import System.Console.GetOpt
    import Data.Maybe (fromMaybe)

    -- the options our program supports
    data Options = Options
            { optAddFileName    :: Bool
            , optFile           :: String
            , optPrefix         :: Maybe String
            } deriving Show

    -- default options of our program
    defaultOptions    = Options
            { optAddFileName    = False
            , optFile           = "source.lua"
            , optPrefix         = Nothing
            }

    -- option table that is used for parsing
    options :: [OptDescr (Options -> Options)]
    options =
        [ Option ['a']      ["Append filename"]
            (NoArg (\opts -> opts { optAddFileName = True }))
            "Appends the file name to the snippet, useful for non-global signatures."
        , Option ['p']      ["Prefix"]
            (ReqArg ((\ f opts -> opts { optPrefix = Just f })) "FILE")
            "Alternative prefix for snippets, useful for changing 'ScenarioUtilities' int 'ScenarioUtils'."
        , Option ['i']     ["Input file"]
            (ReqArg  ((\f opts -> opts { optFile = f })) "FILE")
            "LUA source file to process"
        ]

    -- parses the options if applicable
    compilerOpts :: [String] -> IO (Options, [String])
    compilerOpts argv =
        case getOpt Permute options argv of
            (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
            (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: ic [OPTION...] files..."