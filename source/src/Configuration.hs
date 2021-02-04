
module Configuration (
    readArgs, 
    interpretArgs,
    Configuration (..)

    ) where

    import System.Exit
    import System.FilePath.Posix
    import System.Console.GetOpt

    import Data.Maybe 

    version = 1

    -- the options our program supports
    data Options = Options {

                -- required options --

              optPath           :: Maybe String
            , optType           :: Maybe String

                -- optional options --

            , optAppend         :: Bool
            , optAltContext     :: Maybe String
            , optAltClass       :: Maybe String

            , optDebug          :: Bool
            , optVersion        :: Bool 
            , optHelp           :: Bool
        } deriving Show

    -- default options of our program
    defaultOptions    = Options { 

                -- required options --

              optPath           = Nothing
            , optType           = Nothing

                -- optional options --

            , optAppend         = False
            , optAltContext     = Nothing 
            , optAltClass       = Nothing

            , optDebug          = False
            , optVersion        = False 
            , optHelp           = False
        }

    -- | Program-wide configuration derived from program arguments
    data Configuration = Configuration {

        -- required configuration --

          conPath             :: String
        , conType             :: String 

        -- optional configuration --

        , conAppend           :: Bool 
        , conContext          :: String 
        , conClass            :: String

        , conDebug            :: Bool 

        -- interpreted configuration --

        , conFile             :: String
        , conDirectory        :: String 
    }

    -- option table that is used for parsing
    options :: [OptDescr (Options -> Options)]
    options =
        [ Option ['i']     ["input-file"]
            (ReqArg  ((\f opts -> opts { optPath = Just f })) "(req.)")
            "LUA source file to process"

        , Option ['t']      ["type"]
            (ReqArg ((\ f opts -> opts { optType = Just f })) "(req.)")
            "Type of file. Choices are: 'class', 'function', 'metatable'."

        , Option ['a']      ["append"]
            (NoArg (\opts -> opts { optAppend = True }))
            "Appends the filename as a context, an alternative file name or a class name to the snippet."

        , Option ['f']      ["append-alt-context"]
            (ReqArg ((\ f opts -> opts { optAltContext = Just f })) "")
            "Appends an alternative context in front of the snippet: 'GetMarker(name)' -> 'ScenarioUtilts.GetMarker(name)'"

        , Option ['c']      ["append-alt-class"]
            (ReqArg ((\ f opts -> opts { optAltClass = Just f })) "")
            "Appends an alternative class in front of the snippet: 'GetPosition()' -> 'unit:GetPosition()'"
    
        , Option ['d']      ["debug"]
            (NoArg (\opts -> opts { optDebug = True }))
            "Writes out various intermediate states of the application for debugging."

        , Option ['v']      ["version"]
            (NoArg (\opts -> opts { optVersion = True }))
            "Writes out the program version."

        , Option ['h']      ["help"]
            (NoArg (\opts -> opts { optHelp = True }))
            "Writes out this dialog."
        ]

    -- parses the options if applicable
    readArgs :: [String] -> IO (Options, [String])
    readArgs argv =
        case getOpt Permute options argv of
            (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
            (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "" options))

    --- | Interprets the arguments and raises errors if the arguments are incomplete.
    interpretArgs :: Options -> IO Configuration 
    interpretArgs opts = do 

        case optVersion opts of 
            False -> return ()
            True  -> do putStrLn ("Program version: " ++ show version)
                        exitWith ExitSuccess  

        case optHelp opts of 
            False -> return ()
            True  -> do putStrLn (usageInfo "Help dialog" options)
                        exitWith ExitSuccess 

        -- required arguments --

        conPath <- case optPath opts of 
                Nothing  -> do   let header = "Input file argument (-i) is required."
                                 ioError (userError (usageInfo header options)) 
                (Just s) -> return s

        conType <- case optType opts of 
                Nothing  -> do   let header = "File type argument (-t) is required."
                                 ioError (userError (usageInfo header options)) 
                (Just s) -> return s

        -- conflicting arguments --

        case conType of 
            "class"         -> return ()
            "function"      -> return ()
            "metatable"     -> return ()
            "table"         -> return ()
            _               -> do   let header = "Unknown file type argument (-t): " ++ conType
                                    ioError (userError (usageInfo header options))           

        -- interpreted arguments

        let conDebug = optDebug opts   
        let conFile = takeBaseName conPath
        let conDirectory = dropFileName conPath 

        -- optional arguments --

        let conAppend = optAppend opts

        let conContext = case conAppend of 
                False -> conFile
                True -> fromMaybe conFile (optAltContext opts)

        let conClass = case conAppend of 
                False -> ""
                True -> fromMaybe "NoClassProvided" (optAltClass opts)

        return Configuration {
              conPath       = conPath
            , conType       = conType
            , conAppend     = conAppend
            , conContext    = conContext
            , conClass      = conClass
            , conDebug      = conDebug
            , conFile       = conFile
            , conDirectory  = conDirectory
        }
        