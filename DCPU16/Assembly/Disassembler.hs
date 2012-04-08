module DCPU16.Assembly.Disassembler
    (
    ) where
import DCPU16.Instructions
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Serialize.Get
import Data.Serialize

disassemble :: ByteString -> Vector Instruction
disassemble s = case runGet (getListOf (get::Get Instruction)) s of
    Right is -> V.fromList is
    Left s -> error $ "Disassembly should never, ever, ever fail, but: "++s
