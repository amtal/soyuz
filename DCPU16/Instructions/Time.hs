-- | DCPU-16 instruction execution time in cycles.
module DCPU16.Instructions.Size where
import Instructions
import Data.Word

-- | Instruction execution time in cycles.
--
-- Depends on the instruction, and type of operands used.
--
-- There's also a 1-cycle cost for unsuccessful IF? comparisons, but that can't
-- be predicted without running the code. As such, this prediction is
-- optimistic and assumes all comparisons succeed.
--
-- Since the most cycle-efficient way to write loops, is to jump back on a
-- successful comparison, this prediction should be close to accurate.
time :: Instruction -> Int
time (Basic op a b) = baseCost op + opc a + opc b
time (NonBasic JSR a) = 2 + opc a


-- | Cycle cost regardless of operands.
--
-- Doesn't take 1-cycle failed comparison penalty into account for IF?.
baseCost :: BasicOp -> Int
baseCost op | op `member` [SET,AND,BOR,XOR, IFE,IFN,IFG,IFB] = 1
            | op `member` [ADD,SUB,MUL,SHR,SHL] = 2
            | op `member` [DIV,MOD] = 3

-- | Operand cycle cost.
--
-- Anything that extends instruction length has an extra cycle cost.
--
-- Oddly, there is no cost for accessing indirect addresses. Either a
-- simplification or oversight.
opc :: Operand -> Int
ops (Offset _)  = 1
ops NextIndirect= 1
ops NextDirect  = 1
ops _ = 0
