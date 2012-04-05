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
    ( asm
    , parseFile
    ) where
import Text.Trifecta hiding (Pop,Push)
import Control.Applicative hiding (Const)
import DCPU16.Instructions
import qualified Data.ByteString.Char8 as B
import Debug.Trace

parseFile = parseFromFile asm

asm :: Parser String [Instruction]
asm = spaces >> instructs <* end where
    instructs = many . lexeme . choice $ [instruction, label, comment, dat]
    end = (eof <?> "comment, end of file, or valid instruction")

-- | For now, data only handles one word. 
--
-- Will figure out good syntax sugar (and refactor "asm" to handle multi-word)
-- later.
dat = try $ Data <$ symbol "dat" <*> word

label = Label <$ char ':' <*> labelName <* spaces

labelName = B.pack `fmap` some labelChars

labelChars = alphaNum 

comment = do
    char ';'
    l <- line
    Comment (B.head l == ';') <$> manyTill anyChar eofOrNewline 
  where
    eofOrNewline = ((try newline >> return ()) <|> eof)

instruction :: Parser String Instruction
instruction = choice
    [ Basic <$> basicOp <*> operand <* comma <*> operand
    , NonBasic JSR <$ symbol "jsr" <*> operand
    ]


operand = choice
    [ sym Pop "pop"
    , sym Peek "peek"
    , sym Push "push"
    , sym SP "sp"
    , sym PC "pc"
    , sym O "o"
    , try $ Direct <$> register
    , try $ Indirect <$> brackets register
    , try $ brackets (Offset <$> word <* symbol "+" <*> register)
    , DirectLiteral <$> word
    , IndirectLiteral <$> brackets word
    ]
    
word = lexeme $ choice
    [ Const <$> int
    , LabelAddr <$> labelName
    ]

int = fromIntegral <$> choice
    [ try $ symbol "0" >> hexadecimal
    , decimal
    ]

sym i tok = i <$ symbol tok

register = try $ choice
    [ sym A "a", sym B "b", sym C "c"
    , sym X "x", sym Y "y", sym Z "z"
    , sym I "i", sym J "j"
    ] <* notFollowedBy labelChars

basicOp = choice
    [ sym SET "set", sym ADD "add", sym SUB "sub"
    , sym MUL "mul", sym DIV "div", sym MOD "mod", sym SHL "shl"
    , sym SHR "shr", sym AND "and", sym BOR "bor", sym XOR "xor"
    , sym IFE "ife", sym IFN "ifn", sym IFG "ifg", sym IFB "ifb"
    ]
  where
    sym i tok = i <$ symbol tok
