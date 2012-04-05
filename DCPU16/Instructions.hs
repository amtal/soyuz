-- | Complete abstract description of the DCPU-16 instruction set.
--
-- Based on Version 1.1 of the DCPU-16 Specification by Mojang, retrieved from 0x10c.com.
--
-- Contains a trivial "Label" extension, which has no effect on machine code
-- but is useful for writing assemblers.
module DCPU16.Instructions where
import Data.Word

data Instruction 
    = Basic BasicOp Operand Operand
    | NonBasic NonBasicOp Operand
    | Data Word16
    | Label Word16 -- ^ Not present in machine code, for assembler utility only.
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
    = JSR
    | Reserved Word16 -- ^ Opcode not defined yet.
    deriving (Eq,Read,Show)

-- | Values instructions operate on.
--
-- Direct operands use the value passed to them.
--
-- Indirect operands treat that value as an address for a specific word in memory.
data Operand
    = Direct Register
    | Indirect Register
    | Offset Register
    | Pop | Peek | Push
    | SP | PC 
    | O -- ^ Overflow.
    | NextIndirect
    | NextDirect
    | Literal Word16 -- ^ Restricted to 0x00-0x1f, 5 bits.
    deriving (Eq,Read,Show)

data Register = A|B|C|X|Y|Z|I|J
    deriving (Eq,Read,Show)
