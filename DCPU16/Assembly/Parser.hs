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
    , Options(..)
    , defaults
    ) where
import Text.Trifecta hiding (Pop,Push)
import Control.Applicative hiding (Const)
import DCPU16.Instructions
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Parsing options.
data Options = Options
    { allowUppercase :: Bool 
    -- ^ Parse upper case symbols. Default: off.
    --
    -- Large amounts of assembly get written in lower case. Upper case might
    -- look pretty in small amounts, but holding down the shift key gets old.
    } deriving (Read,Show,Eq)

-- | Default options: lower case only.
defaults = Options False

-- | Parser state. 
--
-- Should factor this into a RWS monad wrapper, but don't understand Trifecta
-- well enough to do so. Monads, how do they work?
data Opt = Opt
    { optSymbols :: (Vector ByteString)
    , options :: Options
    } deriving (Read,Show,Eq)


parseFile opt f = do
    (Just syms) <- parseFromFile symbolDefs f
    parseFromFile (asm (Opt syms opt)) f


symbolDefs :: Parser String (Vector ByteString)
-- | Label definition only parser.
--
-- Meant to be run as the first pass, to extract a table to check label uses
-- against in a future pass.
symbolDefs = process `fmap` (spaces >> ls <* end) where
    -- labels appear at the start of lines: 
    -- if something un-label-like seen, skip to next line
    ls = many . lexeme . choice $ [label,nextLine]
    nextLine = (Data (Const 0)) <$ skipSome (satisfy notMark)
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

end = (eof <?> "comment, end of file, or valid instruction")

-- | For now, data only handles one word. 
--
-- Will figure out good syntax sugar (and refactor "asm" to handle multi-word)
-- later.
dat o = Data <$ symbol "dat" <*> word o

label = Label <$ char ':' <*> labelName <* spaces

labelName = B.pack `fmap` some labelChars

labelChars = alphaNum 

comment = do
    char ';'
    l <- line
    Comment (B.head l == ';') <$> manyTill anyChar eofOrNewline 
  where
    eofOrNewline = ((try newline >> return ()) <|> eof)

instruction :: Opt -> Parser String Instruction
instruction o = choice
    [ Basic <$> basicOp o <*> operand o <* comma <*> operand o
    , NonBasic JSR <$ symbol "jsr" <*> operand o
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
    , try $ Indirect <$> brackets (register o)
    , try $ brackets (Offset <$> word o <* symbol "+" <*> register o)
    , DirectLiteral <$> word o
    , IndirectLiteral <$> brackets (word o)
    ]
    
word :: Opt -> Parser String Word
word o = lexeme $ choice
    [ Const <$> int
    , definedLabel
    ] 
  where
    definedLabel = do
        s <- labelName
        case (s `V.elem` (optSymbols o)) of
            True -> return ()
            False -> err [] $ "label "++show s++" not defined"
        return $ LabelAddr s


int = fromIntegral <$> choice
    [ try $ (char '0' <?> "\"0x\"") >> hexadecimal
    , decimal
    ]

sym o i tok = try $ i <$ string tok <* notFollowedBy labelChars <* spaces

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

