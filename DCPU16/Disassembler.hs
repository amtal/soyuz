-- | Disassembler.
--
-- I should really convert these to use Vector Word16, rather than ByteString.
--
-- It won't be useful for command line tools, but make more sense for
-- everything else.
module DCPU16.Disassembler
    (
    -- * High level disassembly
      disassemble
    -- * Component functions
    , readInstructions
    , annotate
    ) where
import Data.Word (Word16)
import Data.List (sort)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf
import Data.Serialize
import DCPU16.Instructions
import DCPU16.Instructions.Size

-- | Disassembler.
--
-- Annotates result with labels for obvious jumps/calls, but keeps operand
-- addresses as literals.
disassemble :: ByteString -> Vector Instruction
disassemble = annotate . readInstructions
    


-- | Naive disassembly.
--
-- Disassembly can fail at the very end of a buffer, if a partial multi-word
-- instruction is encountered. I'll eventually treat those as data words
-- instead, but for now it crashes... And is a point of weakness to keep in
-- mind for interpreters. What will the official one do?
--
-- It can also produce gibberish if data words are inserted that look like
-- multi-word instructions. This can be hard to catch, and will throw naive
-- disassemblers.
readInstructions :: ByteString -> Vector Instruction
readInstructions s = case runGet (f []) s of
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


-- | Add easily visible labels to instructions.
--
-- Stuff like JSR foo / SET PC, bar / SUB PC, 5
--
-- Labels are not inserted as operands, only defined. The names have the
-- address in them, to match up with operands.
annotate :: Vector Instruction -> Vector Instruction
annotate is = V.fromList . insertLabels ls . V.toList $ is where
    ls :: [(Word16,LabelType)]
    ls = extractLabels is


data LabelType = Call | DirectJump | RelativeJump deriving (Eq,Show,Read,Ord)

-- | Get destinations of all JSR 'calls'.
--
-- Assumes there are no labels, only literals. Any label operands will be
-- skipped.
extractLabels :: Vector Instruction -> [(Word16,LabelType)]
extractLabels = snd . foldl f (0,[]) . V.toList where
    -- TODO: factor out size-fold pattern? how to handle instruction size
    -- failure?
    f (addr,acc) i = (addr',acc') where
        addr' = addr + size i
        acc' = accLabels acc addr' i
    -- labels extracted:
    accLabels acc addr (NonBasic JSR l) | isLit l = 
        (lit l, Call):acc
    accLabels acc addr (Basic SET PC l) | isLit l = 
        (lit l, DirectJump):acc
    accLabels acc addr (Basic ADD PC l) | isLit l = 
        (lit l+addr+1, RelativeJump):acc
    accLabels acc addr (Basic SUB PC l) | isLit l = 
        (lit l+addr+1, RelativeJump):acc
    accLabels acc _ _ = acc
    -- utility
    isLit (DirectLiteral (Const _)) = True
    isLit (ShortLiteral (Const _)) = True
    isLit _ = False
    lit (DirectLiteral (Const w)) = w
    lit (ShortLiteral (Const w)) = w
    lit x = error $ "Not a literal: "++show x

-- | Insert labels.
--
-- Assumes instructions start at 0x0000, and label addresses are only within
-- the range of instructions.
insertLabels :: [(Word16,LabelType)] -> [Instruction] -> [Instruction]
insertLabels ls is = reverse $ f (sort ls) is 0 [] where
    f labels [] _ acc = acc
    f [] is _ acc = reverse is ++ acc
    f (l@(laddr,_):ls) (i:is) addr acc = f labels is addr' acc' where
        addr' = addr+size i
        (acc',labels) = if laddr>=addr && laddr<addr'
            then (i:Label (showLabel l):acc, ls)
            else (i:acc, l:ls)
    showLabel :: (Word16,LabelType) -> ByteString
    showLabel (w,lt) = B.pack $ printf fmt (toInteger w) where
        fmt = case lt of
            Call         -> "func.%04x"
            DirectJump   -> "jump.%04x"
            RelativeJump -> "rel.%04x"

