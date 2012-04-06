{-# LANGUAGE OverloadedStrings #-}
-- | Parser for what seems to be DCPU16 assembly.
--
-- There is some ambiguity between sources: the specification uses uppercase a
-- lot (which I'd rather put in as an option later, with the strict
-- implementation being default).
--
-- A screenshot also shows indirect mode being done with [] instead of (). Go
-- figure.
module DCPU16.Assembler.Parser
    ( parseFile
    , asm
    ) where
import Text.Trifecta hiding (Pop,Push)
import Control.Applicative hiding (Const)
import DCPU16.Instructions
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (guard)
import Data.Generics.Uniplate.Data


-- | Parsing options.
data Opt = Opt
    { optSymbols :: (Vector ByteString)
    } deriving (Read,Show,Eq)

parseFile f = do
    (Just syms) <- parseFromFile symbolDefs f
    parseFromFile (asm (Opt syms)) f


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
    [ Basic <$> basicOp <*> operand o <* comma <*> operand o
    , NonBasic JSR <$ symbol "jsr" <*> operand o
    ]


operand :: Opt -> Parser String Operand
operand o = choice
    [ sym Pop "pop"
    , sym Peek "peek"
    , sym Push "push"
    , sym SP "sp"
    , sym PC "pc"
    , sym O "o"
    , Direct <$> register
    , try $ Indirect <$> brackets register
    , try $ brackets (Offset <$> word o <* symbol "+" <*> register)
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

sym i tok = try $ i <$ string tok <* notFollowedBy labelChars <* spaces

register = try $ choice
    [ sym A "a", sym B "b", sym C "c"
    , sym X "x", sym Y "y", sym Z "z"
    , sym I "i", sym J "j"
    ] 

basicOp = choice
    [ sym SET "set", sym ADD "add", sym SUB "sub"
    , sym MUL "mul", sym DIV "div", sym MOD "mod", sym SHL "shl"
    , sym SHR "shr", sym AND "and", sym BOR "bor", sym XOR "xor"
    , sym IFE "ife", sym IFN "ifn", sym IFG "ifg", sym IFB "ifb"
    ]

