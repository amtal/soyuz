-- | Complete abstract description of the DCPU-16 instruction set.
--
-- Based on Version 1.1 of the DCPU-16 Specification by Mojang, retrieved from 0x10c.com.
--
-- Contains a trivial "Label" extension, which isn't present in machine code
-- but is useful for dealing with assembly.
module DCPU16.Instructions where
import Data.Word hiding (Word)
import Data.ByteString

data Instruction 
    = Basic BasicOp Operand Operand
    | NonBasic NonBasicOp Operand
    | Data Word
    | Label ByteString -- ^ Not present in machine code, for assembler utility only.
    deriving (Eq,Read,Show)

data BasicOp
    = SET
    | ADD | SUB
    | MUL | DIV
    | MOD
    | SHL | SHR
    | AND | BOR | XOR
    | IFE
    | IFN
    | IFG
    | IFB
    deriving (Eq,Read,Show)

data NonBasicOp
    = JSR Word
    | Reserved Word16 -- ^ Opcode not defined yet.
    deriving (Eq,Read,Show)

-- | Values instructions operate on.
--
-- Direct operands use the value passed to them.
--
-- Indirect operands treat that value as an address for a specific word in memory.
data Operand
    = Direct Register
    | Indirect Register -- ^ At address [register].
    | Offset Word Register -- ^ At address [next word + register].
    | Pop | Peek | Push
    | SP | PC 
    | O -- ^ Overflow.
    | IndirectLiteral Word
    | DirectLiteral Word
    | ShortLiteral Word -- ^ Restricted to 0x00-0x1f, 5 bits.
    deriving (Eq,Read,Show)

data Register = A|B|C|X|Y|Z|I|J
    deriving (Eq,Read,Show,Enum)

-- | Constant data.
--
-- Assembly may use adresses of labels to initialize such data: since the
-- address may not be known immediately, the label extension is added.
data Word 
    = Const Word16 
    | LabelAddr ByteString 
    deriving (Eq,Read,Show)

