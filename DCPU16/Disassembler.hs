module DCPU16.Disassembler
    ( disassemble
    ) where
import DCPU16.Instructions
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Serialize

-- | Disassembler.
--
-- Disassembly can fail at the very end of a buffer, if a partial multi-word
-- instruction is encountered. I'll eventually treat those as data words
-- instead, but for now it crashes... And is a point of weakness to keep in
-- mind for interpreters. What will the official one do?
disassemble :: ByteString -> Vector Instruction
disassemble s = case runGet (f []) s of
    Right is -> V.fromList . reverse $ is
    Left s -> error $ "Disassembly should never, ever, ever fail, but: "++s
  where
    f acc = do
        i <- get :: Get Instruction
        n <- remaining
        let next = case n of
                    0 -> return
                    _ -> f
        next (i:acc)
