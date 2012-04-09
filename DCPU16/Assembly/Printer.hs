-- | DCPU-16 pretty-printing.
--
-- Meant for printing disassembly, debugger data, and other machine output.
--
-- Focus is on consistency and predictability, not special snowflake
-- indentation or ASCII art.
--
-- Should be compatible with other people's stuff, with two caveats:
--  
--  * I saw a screenshot that used () for indirect rather than [].
--
--  * Output is lower case.
module DCPU16.Assembly.Printer 
    ( pprint
    ) where
import DCPU16.Instructions
import Text.PrettyPrint
import Data.Char (toLower)
import Data.ByteString.Char8 (unpack)
import Text.Printf
import Data.Word (Word16)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Nicely formatted ASCII output.
pprint :: Vector Instruction -> String
pprint = render . V.foldl pI empty

pI :: Doc -> Instruction -> Doc
pI acc (Comment solo xs) 
    | not solo   = acc $$ nest 40 (semi <> text xs)
    | otherwise  = acc $+$ semi <> text xs
pI acc (Label s) = acc $$ colon <> text (unpack s)
pI acc (Data x)  = acc $$ nest 16 (text "dat" <+> pW x)
pI acc (Basic op a b) = acc $$ nest 16 (pBO op <+> pO a <> comma <+> pO b)
pI acc (NonBasic op a)= acc $$ nest 16 (pNBO op <+> pO a)

pW (Const x) | x<0xa     = text . show $ x
             | otherwise = pHex "0x%x" x
pW (LabelAddr s) = text . unpack $ s

pBO :: BasicOp -> Doc
pBO = text . map toLower . show

pNBO :: NonBasicOp -> Doc
pNBO JSR = text "jsr"
pNBO (Reserved code) = pHex "; RESERVED INSTRUCTION: 0x%x" code

pO :: Operand -> Doc
pO (Direct r) = pReg r
pO (Indirect r) = brackets $ pReg r
pO (Offset off r) = brackets $ pW off <> char '+' <> pReg r
pO (IndirectLiteral w) = brackets $ pW w
pO (DirectLiteral w) = pW w
pO (ShortLiteral w) = pW w
pO o = text . map toLower . show $ o

pHex :: String -> Word16 -> Doc
pHex fmt w = text (printf fmt w)

pReg = text . map toLower . show
