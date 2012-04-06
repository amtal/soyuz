-- | Simple assembly optimizations.
--
-- Focus on small tweaks, like shortening instructions with small literals, 
-- and any NOP\/call\/arithmetic optimizations that come up.
module DCPU16.Assembly.Optimizer 
    ( sizeVariant
    ) where
import DCPU16.Instructions
import DCPU16.Instructions.Size
import Data.Maybe (maybe)
import Data.Word (Word16)
import Data.ByteString
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Generics.Uniplate.Data
import qualified Data.Map as M

-- | Optimizations that change the size of instructions, but not order.
--
-- Should be run before label-to-address translation.
sizeVariant :: Vector Instruction -> Vector Instruction
sizeVariant = shortLabelLiterals -- compress any low label adresses
            . shortLiterals -- compress the easy bits

shortLiterals :: Vector Instruction -> Vector Instruction
-- ^ Rewrite small constants to make the instructions one word shorter.
--
-- Only works for literals <=32.
shortLiterals = V.map (rewriteBi f) where
    f (DirectLiteral c@(Const w)) | w<=0x1f = Just $ ShortLiteral c
    f _ = Nothing

shortLabelLiterals :: Vector Instruction -> Vector Instruction
-- ^ Rewrite small label addresses to make instructions one word shorter.
--
-- Only works for addresses <=32 (not many) and vulnerable to size changes.
--
-- Should probably be one of the last steps.
shortLabelLiterals is = rewriteBi f `V.map` is where
    f (DirectLiteral l@(LabelAddr s)) | addr s<=0x1f = Just $ ShortLiteral l
    f _ = Nothing
    addr :: ByteString -> Word16
    addr s = maybe (error $ "undefined label "++show s) id (M.lookup s lut)
    lut = M.fromList . snd $ V.foldl fun (0,[]) is where
        fun (addr,acc) (Label s) = (addr,(s,addr):acc)
        fun (addr,acc) i = (addr + size i, acc)
