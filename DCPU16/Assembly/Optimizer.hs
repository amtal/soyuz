-- | Simple assembly optimizations.
--
-- Focus on small tweaks, like shortening instructions with small literals, 
-- and any NOP\/call\/arithmetic optimizations that come up.
module DCPU16.Assembly.Optimizer 
    ( Optimization(..)
    , sizeVariant
    ) where
import DCPU16.Instructions
import DCPU16.Instructions.Size
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Data.Bits

-- | Optimizations, listed in the order they're performed. (Earliest first.)
data Optimization
    -- | Large number (0xffe1+) add\<-\>sub overflow.
    --
    -- Saves 1 word of space when adding/subtracting large numbers.
    --
    -- > add a,0xffff   ; sub,a,1
    -- > add a,0xffe1   ; sub a,0x1f
    -- > add a,0xffe0   ; no longer fits into short literal
    -- > sub a,0xffff   ; add a,1 (trivial dual)
    --
    -- Caution: O register behaves very differently. As such, this optimization
    -- is default-off.
    = AddSubOverflow
    -- | Rewrites power of two multiplies and divides as shifts.
    --
    -- > mul a,256      ; shl a,8 (one word shorter)
    -- > mul a,16       ; untouched
    -- > div a,128      ; shr,a,7
    --
    -- Multiply and shift are both two cycles. It should only be rewritten if
    -- the original doesn't fit into into a short literal (removing a word-load
    -- cycle).
    --
    -- Division is always rewritten, being 3 cycles long.
    --
    -- Effect on O should be identical.
    | PowerOfTwoMulDiv
    -- | Shortens instructions by 1 word if they use literals smaller than 0x20.
    --
    -- > set a,1            ; 1 word, not 2
    -- > add [0x8000],1     ; 2 words, not 3
    | ShortLiterals
    -- | Performs the short literal optimization on labels.
    --
    -- > :l_0000    set pc,l.0000       ; 1 word, address < 0x20
    -- > :l_8000    set pc,l.8000       ; 2 words, large address
    | ShortLabelLiterals
    deriving (Eq,Read,Show,Ord,Enum,Bounded)

type Step = Vector Instruction -> Vector Instruction

-- | Compose a series of optimizations.
--
-- First in the list = first run. Last = last.
pipeline :: [(Optimization,Step)] -> Step
pipeline = foldl1 (flip (.)) . map snd

-- | Optimizations that change the size of instructions, but not order.
--
-- Should be run before label-to-address translation.
sizeVariant :: Step
sizeVariant = pipeline
    [(AddSubOverflow, addSubOverflow)
    ,(PowerOfTwoMulDiv, mulDivP2)
    ,(ShortLiterals, shortLiterals)
    ,(ShortLabelLiterals, shortLabelLiterals)
    ]



--
-- Actual optimization implementations follow:
--


addSubOverflow :: Step
addSubOverflow = V.map f where
    f (Basic op a b) | (op==ADD || op==SUB) && isLit b && lit b >= 0xffe1 
        = Basic (switch op) a . DirectLiteral . Const $ 0 - lit b
    f x = x
    switch ADD = SUB
    switch SUB = ADD

mulDivP2 :: Step
mulDivP2 = V.map f where
    f (Basic MUL a b) | isLit b && isPow2 b && lit b >= 0x20 
        = Basic SHL a . DirectLiteral . Const . log2 $ b
    f (Basic DIV a b) | isLit b && isPow2 b 
        = Basic SHR a . DirectLiteral . Const . log2 $ b
    f x = x
    isPow2 o = isLit o && w.&.(w-1)==0 && w/=0 -- fast algorithm from wikipedia
      where w = lit o
    -- there are nice log2 algorithms at:
    -- <http://graphics.stanford.edu/~seander/bithacks.html#IntegerLogObvious>
    -- however I don't see anything I can implement cleanly :p
    log2 = r 0 . lit  where 
        r e 0 = e
        r e n = r (e+1) (shiftR n 1)

isLit :: Operand -> Bool
isLit (DirectLiteral _) = True
isLit (ShortLiteral _) = True
isLit _ = False

lit :: Operand -> Word16
lit (DirectLiteral (Const w)) = w
lit (ShortLiteral (Const w)) = w
lit x = error $ "Not a literal: "++show x



shortLiterals :: Vector Instruction -> Vector Instruction
-- ^ Rewrite small constants to make the instructions one word shorter.
--
-- Only works for literals <=32.
shortLiterals = V.map (transformBi f) where
    f (DirectLiteral c@(Const w)) | w<=0x1f = ShortLiteral c
    f x = x

shortLabelLiterals :: Vector Instruction -> Vector Instruction
-- ^ Rewrite small label addresses to make instructions one word shorter.
--
-- Only works for addresses <=32 (not many) and vulnerable to size changes.
--
-- Should probably be one of the last steps.
shortLabelLiterals is = transformBi f `V.map` is where
    f (DirectLiteral l@(LabelAddr s)) | addr s<=0x1f = ShortLiteral l
    f x = x
    addr :: ByteString -> Word16
    addr s = fromMaybe (error $ "undefined label "++show s) (M.lookup s lut)
    lut = M.fromList . snd $ V.foldl fun (0,[]) is where
        fun (addr,acc) (Label s) = (addr,(s,addr):acc)
        fun (addr,acc) i = (addr + size i, acc)
