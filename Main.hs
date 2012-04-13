{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.Exit
import System.IO (hPutStrLn, stderr)
import qualified DCPU16.Assembly.Parser as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad (when, void)
import DCPU16.Assembly.Printer
import DCPU16.Assembly.Optimizer
import DCPU16.Assembler
import DCPU16.Disassembler
import DCPU16.Hex

main = do
    opts <- cmdArgs options
    instr <- readInput opts
    let out = processOutput opts . optimize opts $ instr
    case output opts of
        -- absolutely do not do 'putStrLn', to not break binary input:
        "" -> B.putStr out 
        f -> B.writeFile f out
  where
    packNlEof s = B.snoc (B.pack s) '\n'
    readInput opts = do
        instr <- case runMode opts of
            Disassemble -> do
                s <- B.readFile (inputFile opts)
                return . Just . disassemble $ s
            _ -> do
                let po = P.defaults
                        { P.roundedBrackets = parseSmoothBrackets opts
                        }
                P.parseFile po (inputFile opts)
        case instr of
            Just is -> return is
            Nothing -> exitFailure -- parser already printed messages
    optimize opts is = if runMode opts/=Assemble || noOptimization opts 
        then is 
        else sizeVariant is
    processOutput opts instr = case runMode opts of
        Assemble -> binEncoding . assemble $ instr
        _ -> packNlEof $ pprint instr
      where
        binEncoding = if hexdump opts then packNlEof . dumpBytes else id

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
    , parseSmoothBrackets :: Bool
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
        &= help "Disable short literal/label optimization"
        &= groupname "Optimization"
    , output = def &= typ "<FILE>"
        &= help "Write to file instead of stdout"
        &= groupname "General"
    , hexdump = False &= help "Encode binary data in a 16-bit hexdump"
    , parseSmoothBrackets = False
        &= explicit &= name "smooth-brackets"
        &= help "Parse (a) instead of [a] for indirect mode"
    } &= program "soyuz"
      &= summary "Сойуз 0.0.0, amtal <alex.kropivny@gmail.com>"
      &= details
    [ "Documentation and source at https://github.com/amtal/soyuz or on Hackage."
    ]
