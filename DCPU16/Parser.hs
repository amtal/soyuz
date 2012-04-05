{-# LANGUAGE OverloadedStrings #-}
import Text.Trifecta hiding (Pop,Push)
import Control.Applicative hiding (Const)
import DCPU16.Instructions
import Data.ByteString.Char8

test = parseFromFile asm

asm = spaces >> (many . lexeme . choice $ [instruction, label, comment]) <* eof

label = Label <$ char ':' <*> labelName <* spaces

labelName = pack `fmap` some labelChars

labelChars = alphaNum 

comment = Comment <$ char ';' <*> manyTill anyChar eofOrNewline where
    eofOrNewline = ((try newline >> return ()) <|> eof)

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
