{-# LANGUAGE OverloadedStrings #-}
-- | Parser for what seems to be DCPU16 assembly.
--
-- There is some ambiguity between sources: the specification uses uppercase a
-- lot (which I'd rather put in as an option later, with the strict
-- implementation being default).
--
-- A screenshot also shows indirect mode being done with [] instead of (). Go
-- figure.
module DCPU16.Assembly.Parser
    ( parseFile
    , defaults
    , Options(..)
    ) where
import Text.Trifecta hiding (Pop,Push)
import Control.Applicative hiding (Const)
import DCPU16.Instructions
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import Data.Word (Word16)
import qualified Data.Vector as V
import Data.Char (toUpper)
import Control.Monad (void,unless,when)
import Text.Printf

-- | Default parsing options.
defaults :: Options
defaults = Options False

-- | Parsing options, if you want to override the defaults.
data Options = Options
    { roundedBrackets :: Bool
    -- ^ Indirect mode via \(\) instead of \[\]. Default: off.
    --
    -- Weird, but showed up in a screenshot.
    } deriving (Read,Show,Eq)


-- | Parser state. 
--
-- Should factor this into a RWS monad wrapper, but don't understand Trifecta
-- well enough to do so. Monads, how do they work?
data Opt = Opt
    { optSymbols :: Vector ByteString
    , options :: Options
    } deriving (Read,Show,Eq)


-- | Parse a file.
--
-- Detailed  errors with line and column numbers (as well as expected values)
-- will be printed to console if parsing fails.
parseFile opt f = do
    ss <- parseFromFile symbolDefs f 
    case ss of
        (Just syms) -> parseFromFile (asm (Opt syms opt)) f
        Nothing -> return Nothing


symbolDefs :: Parser String (Vector ByteString)
-- | Label definition only parser.
--
-- Meant to be run as the first pass, to extract a table to check label uses
-- against in a future pass.
symbolDefs = process `fmap` (spaces >> ls <* end) where
    -- labels appear at the start of lines: 
    -- if something un-label-like seen, skip to next line
    ls = many . lexeme . choice $ [label,nextLine]
    nextLine = Data (Const 0) <$ skipSome (satisfy notMark)
    notMark c = c/='\n'
    process = V.fromList . map (\(Label s)->s) . filter isLabel
    isLabel (Label _) = True
    isLabel _ = False


-- | Instruction, comment, and label parser.
--
-- Relies on a symbol table parsed in a previous pass to check for label
-- existance.
asm :: Opt -> Parser String (Vector Instruction)
asm o = V.fromList `fmap` (spaces >> instructs <* end) where
    instructs = many . lexeme . choice $ [instruction o, label, comment, dat o]

end = eof <?> "comment, end of file, or valid instruction"

-- | For now, data only handles one word. 
--
-- Will figure out good syntax sugar (and refactor "asm" to handle multi-word)
-- later.
dat o = Data <$ symbol "dat" <*> word o

label = do
    char ':' 
    l<-labelName 
    when (isRegName (B.unpack l)) $
      err [] $ "label definition "++show l++" clashes with register name"
    spaces
    return $ Label l
  where
    isRegName :: String -> Bool
    isRegName s = map toUpper s `elem` regs
    regs = ["A","B","C","X","Y","Z","I","J"
           ,"POP","PEEK","PUSH","PC","O"
           ]

labelName = B.pack `fmap` some labelChars

labelChars = alphaNum <|> char '_' <|> char '.'

comment = do
    char ';'
    l <- line
    Comment (B.head l == ';') <$> manyTill anyChar eofOrNewline 
  where
    eofOrNewline = void (try newline) <|> eof

instruction :: Opt -> Parser String Instruction
instruction o = choice
    [ Basic <$> basicOp o <*> operand o <* comma <*> operand o
    , NonBasic <$> sym o JSR "jsr" <*> operand o
    ]


operand :: Opt -> Parser String Operand
operand o = choice
    [ sym o Pop "pop"
    , sym o Peek "peek"
    , sym o Push "push"
    , sym o SP "sp"
    , sym o PC "pc"
    , sym o O "o"
    , Direct <$> register o
    , DirectLiteral <$> word o
    , brace o (indirect o)
    ]
  where
    indirect o = choice
        [ try $ Offset <$> word o <* symbol "+" <*> register o
        , try $ flip Offset <$> register o <* symbol "+" <*> word o
        , try $ Indirect <$> register o
        , IndirectLiteral <$> word o
        ]


-- This code is based on the Haskell parser, and thus strips a lot more
-- whitespace than desired. [\na+2] probably shouldn't be valid assembly.
brace o = if roundedBrackets $ options o 
    then nesting . between (symbolic '(') (symbolic ')')
    else brackets
 
word :: Opt -> Parser String Word
word o = lexeme $ choice
    [ Const <$> int
    , definedLabel
    ] 
  where
    definedLabel = do
        s <- labelName
        unless (s `V.elem` optSymbols o) $
            err [] $ "label "++show s++" not defined"
        return $ LabelAddr s


int :: Parser String Word16
int = fromInteger <$> (num >>= checkSize)
  where
    num = choice
        [ try $ (char '0' <?> "\"0x\"") >> hexadecimal
        , decimal
        ]
    checkSize :: Integer -> Parser String Integer
    checkSize n = if n>0xffff 
        then do
            err [] (printf fmt n)
            return (mod n 0xffff)
        else return n
    fmt = "literal 0x%x is wider than 16 bits"


sym o i tok = try $ i <$ token <* notFollowedBy labelChars <* spaces 
  where
    token = string tok <|> string (map toUpper tok)

register o = try $ choice
    [ sym o A "a", sym o B "b", sym o C "c"
    , sym o X "x", sym o Y "y", sym o Z "z"
    , sym o I "i", sym o J "j"
    ] 

basicOp o = choice
    [ sym o SET "set", sym o ADD "add", sym o SUB "sub"
    , sym o MUL "mul", sym o DIV "div", sym o MOD "mod", sym o SHL "shl"
    , sym o SHR "shr", sym o AND "and", sym o BOR "bor", sym o XOR "xor"
    , sym o IFE "ife", sym o IFN "ifn", sym o IFG "ifg", sym o IFB "ifb"
    ]

