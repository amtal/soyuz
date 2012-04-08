{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Exit
import System.IO (hPutStrLn, stderr)
import qualified DCPU16.Assembly.Parser as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import DCPU16.Assembly.Printer
import DCPU16.Assembly.Optimizer
import DCPU16.Assembler
import DCPU16.Disassembler
import DCPU16.Hex

main = do
    opts <- cmdArgs options
    -- read input
    Just instr <- case runMode opts of
        Disassemble -> do
            s <- B.readFile (inputFile opts)
            return . Just . disassemble $ s
        _ -> do
            let po = P.defaults
                    { P.allowUppercase = parseUpperCase opts
                    , P.roundedBrackets = parseSmoothBrackets opts
                    }
            P.parseFile po (inputFile opts)
    -- optimize
    let instr' = case noOptimization opts of
            True -> instr
            False -> sizeVariant instr
    -- print output
    let binEncoding = if hexdump opts then dumpBytes else B.unpack
        out = case runMode opts of
            Assemble -> binEncoding . assemble $ instr'
            _ -> pprint instr'
    case output opts of
        "" -> putStrLn out 
        f -> writeFile f out

-- | Print a message to stderr and exit.
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitFailure

data RunMode = Assemble | Disassemble | PrettyPrint
    deriving (Eq,Show,Read,Data,Typeable)

data Options = Options
    { runMode :: RunMode
    , inputFile :: String
    , noOptimization :: Bool
    , output :: String
    , hexdump :: Bool
    , parseUpperCase, parseSmoothBrackets :: Bool
    } deriving (Eq,Show,Read,Data,Typeable)

options = Options 
    { runMode = enum
        [ PrettyPrint&= help "Assembly -> consistently formatted assembly"
        , Assemble   &= help "Assembly -> machine code"
        , Disassemble&= help "Machine code -> assembly"
        ] &= groupname "Mode of operation"
    , inputFile = "" &= argPos 0 &= typ "<FILE>"
    , noOptimization = False
        &= explicit &= name "no-optimize"
        &= help "Disable short literal optimization"
        &= groupname "Optimization"
    , output = def &= typ "<FILE>"
        &= help "Write to file instead of stdout"
        &= groupname "General"
    , hexdump = False &= help "Encode binary data in a 16-bit hexdump"
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
