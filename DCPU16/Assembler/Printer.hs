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
module DCPU16.Assembler.Printer 
    ( pprint
    ) where
import DCPU16.Instructions
import Text.PrettyPrint
import Data.Char (toLower)
import Data.ByteString.Char8 (unpack)
import Text.Printf
import Data.Word (Word16)

-- | Nicely formatted ASCII output.
pprint :: [Instruction] -> String
pprint = render . foldr pI empty

pI :: Instruction -> Doc -> Doc
pI (Comment solo xs) acc= acc $$ semi <> text xs
pI (Label s) acc        = acc $$ colon <> text (unpack s)
pI (Data x) acc         = acc $$ nest 16 (text "dat" <+> pW x)
pI (Basic op a b) acc   = acc $$ nest 16 (pBO op <+> pO a <> comma <+> pO b)
pI (NonBasic op a) acc  = acc $$ nest 16 (pNBO op <+> pO a)

pW (Const x) = pHex "0x%x" x
pW (LabelAddr s) = text $ show s

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
