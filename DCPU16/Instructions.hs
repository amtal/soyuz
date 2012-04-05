{-# LANGUAGE DeriveDataTypeable #-}
-- | Complete abstract description of the DCPU-16 instruction set.
--
-- Based on Version 1.1 of the DCPU-16 Specification by Mojang, retrieved from 0x10c.com.
--
-- Contains a trivial 'Label' extension, which isn't present in machine code
-- but is useful for dealing with assembly.
module DCPU16.Instructions
    ( Instruction(..), BasicOp(..), NonBasicOp(..), Operand(..), Register(..), Word(..)
    ) where
-- Fundamental types
import Data.Word hiding (Word)
import Data.Data
import Data.ByteString
-- Serialization
import Data.Serialize
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Word hiding (Word)
import Control.Applicative hiding (Const)

-- | Abstract DCPU-16 instruction set.
--
-- Can be read and written as machine code via the Serialize instance.
data Instruction 
    = Basic BasicOp Operand Operand
    | NonBasic NonBasicOp Operand
    | Data Word
    | Label ByteString -- ^ Not present in machine code, for assembler utility only.
    | Comment Bool String -- ^ Boolean true if comment is alone on its own line.
    deriving (Eq,Read,Show,Data,Typeable)

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
    deriving (Eq,Read,Show,Data,Typeable)

data NonBasicOp
    = JSR
    | Reserved Word16 -- ^ Opcode not defined yet.
    deriving (Eq,Read,Show,Data,Typeable)

-- | Values instructions operate on.
--
-- Direct operands use the value passed to them.
--
-- Indirect operands treat that value as an address for a specific word in memory.
data Operand
    = Direct Register
    | Indirect Register -- ^ At address [register].
    | Offset Word Register -- ^ At address [next word + register].
    | Pop | Peek | Push
    | SP | PC 
    | O -- ^ Overflow.
    | IndirectLiteral Word
    | DirectLiteral Word
    | ShortLiteral Word -- ^ Restricted to 0x00-0x1f, 5 bits.
    deriving (Eq,Read,Show,Data,Typeable)

data Register = A|B|C|X|Y|Z|I|J
    deriving (Eq,Read,Show,Data,Typeable,Enum)

-- | Constant data.
--
-- Assembly may use adresses of labels to initialize such data: since the
-- address may not be known immediately, the label extension is added.
data Word 
    = Const Word16 
    | LabelAddr ByteString 
    deriving (Eq,Read,Show,Data,Typeable)




-- Machine language encoding/decoding follows.
--
-- Rather crude for now, needs refactoring to use a bit-aware packer. And
-- general cleanup.

-- | Machine code encoding/decoding.
instance Serialize Instruction where
    put (Basic op a b) = do
        let (a',aw) = packOp a
            (b',bw) = packOp b
        putWord16be $ (((b' `shiftL` 6) .|. a') `shiftL` 4) .|. fromOpCode op
        maybe (return ()) putWord16be aw
        maybe (return ()) putWord16be bw
    put (NonBasic op a) = putNonBasic opCode a
      where
        opCode = case op of JSR->0x01; Reserved x->0x3f.&.x
        putNonBasic op a = do 
            let (a',w) = packOp a
            putWord16be $ shiftL ((shiftL a' 6) .|. op) 4
            maybe (return ()) putWord16be w
    put (Data x) = put x
    put (Label s) = return ()
    get = do
        w <- getWord16be
        let [b,a,op] = fmap (w.&.) [0xfc00, 0x03f0, 0x000f]
        if op==0 then
            NonBasic (getNBCode a) <$> getOp b
                else
            Basic (toOpCode op) <$> getOp a <*> getOp b


instance Serialize Word where
    put (LabelAddr s) = fail $ "can not serialize label address "++show s
    put (Const x) = putWord16be x
    get = Const <$> getWord16be

getNBCode 0x01 = JSR
getNBCode op = Reserved op

-- | Parse 6-bit operand.
getOp :: Word16 -> Get Operand
getOp op | op<=0x0f = getRegMode op
         | op==0x1e = IndirectLiteral <$> get
         | op==0x1f = DirectLiteral <$> get
         | op>=0x20 = return $ (ShortLiteral . Const) (op-0x20)
         | otherwise = return $ toOperand op

-- | Pack 6-bit operand and any additional word it has.
packOp :: Operand -> (Word16, Maybe Word16)
packOp = undefined

getRegMode :: Word16 -> Get Operand
getRegMode op | op `testBit` 3 = do offset <- get
                                    return (Offset offset r)
              | op `testBit` 4 = return (Indirect r)
              | otherwise = return (Direct r)
  where
    r = toEnum . fromIntegral $ op .&. 0x7

toOperand 0x18 = Pop
toOperand 0x19 = Peek
toOperand 0x1a = Push
toOperand 0x1b = SP
toOperand 0x1c = PC
toOperand 0x1d = O

fromOpCode SET=0x1; fromOpCode ADD=0x2; fromOpCode SUB=0x3
fromOpCode MUL=0x4; fromOpCode DIV=0x5; fromOpCode MOD=0x6; fromOpCode SHL=0x7
fromOpCode SHR=0x8; fromOpCode AND=0x9; fromOpCode BOR=0xa; fromOpCode XOR=0xb
fromOpCode IFE=0xc; fromOpCode IFN=0xd; fromOpCode IFG=0xe; fromOpCode IFB=0xf

toOpCode 0x1=SET; toOpCode 0x2=ADD; toOpCode 0x3=SUB
toOpCode 0x4=MUL; toOpCode 0x5=DIV; toOpCode 0x6=MOD; toOpCode 0x7=SHL
toOpCode 0x8=SHR; toOpCode 0x9=AND; toOpCode 0xa=BOR; toOpCode 0xb=XOR
toOpCode 0xc=IFE; toOpCode 0xd=IFN; toOpCode 0xe=IFG; toOpCode 0xf=IFB 
