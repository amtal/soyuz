{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Exit
import System.IO (hPutStrLn, stderr)

main = do
    opts <- cmdArgs options
    putStrLn "ok."

-- | Print a message to stderr and exit.
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitFailure

data RunMode = Assemble | Disassemble | PrettyPrint
    deriving (Eq,Show,Read,Data,Typeable)

data Options = Options
    { runMode :: RunMode
    , inputFile :: String
    , noOptimization :: Bool
    , outputFile :: String
    , parseUpperCase, parseSmoothBrackets :: Bool
    } deriving (Eq,Show,Read,Data,Typeable)

options = Options 
    { runMode = enum
        [ Assemble   &= help "Assembly -> machine code"
        , Disassemble&= help "Machine code -> assembly"
        , PrettyPrint&= help "Assembly -> consistently formatted assembly"
        ] &= groupname "Mode of operation"
    , inputFile = def &= argPos 0 &= typ "<FILE>"
    , noOptimization = False
        &= explicit &= name "no-optimize"
        &= help "Disable short literal optimization"
        &= groupname "Optimization"
    , outputFile = def &= explicit &= name "output" &= typ "<FILE>"
        &= help "Write to file instead of stdout"
        &= groupname "General"
    , parseUpperCase = False
        &= explicit &= name "uppercase"
        &= help "Parse uppercase symbols (but never mixed case)"
    , parseSmoothBrackets = False
        &= explicit &= name "smooth-brackets"
        &= help "Parse (a) instead of [a] for indirect mode"
    } &= program "0x10c"
      &= summary "0x10c 0.0.0, amtal <alex.kropivny@gmail.com>"
      &= details
    [ "Documentation and source at https://github.com/amtal/0x10c or on Hackage."
    ]
