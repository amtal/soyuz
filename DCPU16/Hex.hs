-- | A weird utility module, with stuff I don't know where to put.
--
-- I didn't call it Util because that's just a slippery slope.
module DCPU16.Hex ( 
    -- * Hexdumps
      dump, dumpBytes
    -- * Homeless utility functions
    , warn, getWord16s
    ) where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector (Vector)
import qualified Data.Vector as V
-- printing warnings to stderr:
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
-- bytestring->V word16
import Data.Serialize
import Data.Word (Word16)

-- | Prints a message to stderr.
--
-- Inserts newlines automatically. Pretends it doesn't perform IO.
warn :: String -> a -> a
warn s a = unsafePerformIO (hPutStrLn stderr s >> return a)


-- | Prints a nice dump of 16-bit words.
--
-- >>> putStrLn . dump . fromList $ [1..32]
-- 0000: 0001 0002 0003 0004 0005 0006 0007 0008
-- 0008: 0009 000a 000b 000c 000d 000e 000f 0010
-- 0010: 0011 0012 0013 0014 0015 0016 0017 0018
-- 0018: 0019 001a 001b 001c 001d 001e 001f 0020
--
-- Complexity is probably terrible, but when people write programs big enough
-- for it to matter, I'll fix it.
dump :: Vector Word16 -> String
dump = concat . V.toList . V.imap pAddr
  where
    pAddr :: Int -> Word16 -> String
    pAddr 0 = ("0000:"++) . pWord
    pAddr n | mod n 8 == 0 = (printf "\n%04x:" n++) . pWord
            | otherwise = pWord
    pWord :: Word16 -> String
    pWord = printf " %04x" . toInteger


-- | ByteString version of 'dump'.
--
-- If faced with odd number of bytes, truncates the last one and prints a
-- warning to stderr.
dumpBytes :: ByteString -> String
dumpBytes s = dump . getWord16s $ s where
    len = B.length s
    s' = case len `mod` 2 of
            1 -> warn msg $ B.init s
            0 -> s
    msg = "Warning: truncating last byte of "++show len++" byte hex string before\n"
       ++ "         dumping. Dump works on 16-bit words only."

-- | Utility for parsing bytestrings as word lists.
--
-- >>> getWord16s . pack $ "asdfasdf"
-- fromList [24947,25702,24947,25702]
--
-- If length in bytes is odd, discards last byte silently.
getWord16s :: ByteString -> Vector Word16
getWord16s s = V.fromList . reverse . either err id $ runGet (f len []) s
  where
    len = B.length s `div` 2
    f 0 acc = return acc
    f n acc = do
        w <- getWord16be
        f (n-1) (w:acc)
    err e = error $ "getWord16s impossible error: "++e
