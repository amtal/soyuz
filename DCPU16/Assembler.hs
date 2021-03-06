-- | Assembler.
--
-- Works with "DCPU16.Instructions". Does no optimizations, just converts
-- labels to direct addresses and produces machine code.
--
-- Needs to be extended to support relative addressing. Extending labels to do
-- arithmetic: @sub pc,this-loop@ is a necessary step. I see this as more of an
-- optimization problem, since these kinds of rewrites would be neat to perform
-- automatically...
--
-- We'll see which direction it gets approached from.
module DCPU16.Assembler 
    ( assemble
    -- * Utility
    , labelAddrs
    ) where
import DCPU16.Instructions
import DCPU16.Instructions.Size
import Data.Word (Word16)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Data.Map (Map)
import Data.Serialize

-- | Turn labels into direct addresses, and output machine code.
assemble :: Vector Instruction -> ByteString
assemble = B.concat . V.toList . V.map (runPut . put) . labelsToAddrs


-- | Build address lookup table for label definitions.
labelAddrs :: Vector Instruction -> Map ByteString Word16
labelAddrs = snd . V.foldl f (0,M.empty) where
    f (addr,map) i = (addr',map') where
        addr' = addr + size i
        map' = case i of
            (Label s) -> M.insert s addr map
            _ -> map

-- | Replace every reference to a label with an address.
--
-- Strips comments and label definitions as well, assuming they won't be needed.
labelsToAddrs :: Vector Instruction -> Vector Instruction
labelsToAddrs is = 
    let is' =  V.filter (not . isComment) is
        lut = labelAddrs is'
    in  V.map (remap lut) . V.filter (not . isLabel) $ is'
  where
    isComment (Comment _ _) = True
    isComment _ = False
    isLabel (Label _) = True
    isLabel _ = False
    remap lut = transformBi f where
        f (LabelAddr s) = case M.lookup s lut of
            Just addr -> Const addr
            Nothing -> error $ "Undefined label: "++show s
        f x = x
