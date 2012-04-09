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

-- | Optimizations, listed in the order they're performed. (Earliest first.)
data Optimization
    -- | Shortens instructions by 1 word if they use literals smaller than 0x20.
    --
    -- > set a,1            ; 1 word, not 2
    -- > add [0x8000],1     ; 2 words, not 3
    = ShortLiterals
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
    [(ShortLiterals, shortLiterals)
    ,(ShortLabelLiterals, shortLabelLiterals)
    ]


--
-- Actual optimization implementations follow:
--

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
    addr s = fromMaybe (error $ "undefined label "++show s) (M.lookup s lut)
    lut = M.fromList . snd $ V.foldl fun (0,[]) is where
        fun (addr,acc) (Label s) = (addr,(s,addr):acc)
        fun (addr,acc) i = (addr + size i, acc)
