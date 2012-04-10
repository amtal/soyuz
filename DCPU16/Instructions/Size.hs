-- | DCPU-16 instruction size in memory.
module DCPU16.Instructions.Size 
    ( size
    ) where
import DCPU16.Instructions
import Data.Word

-- | Instruction size in words.
--
-- Each word is 16 bits. Instructions are always 1, 2, or 3 words in length.
-- 
-- Comments and labels don't appear in machine code.
size :: Instruction -> Word16
size (Basic _ a b)  = 1 + ops a + ops b
size (NonBasic _ a) = 2 + ops a
size (Comment _ _) = 0
size (Label _) = 0
size (Data _) = 1

-- | Operand size.
ops :: Operand -> Word16
ops (Offset _ _)     = 1
ops (IndirectLiteral _) = 1
ops (DirectLiteral _)   = 1
ops _ = 0
