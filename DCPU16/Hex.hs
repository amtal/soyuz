module DCPU16.Hex
    ( dump, dumpBytes
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
dumpBytes s = dump . V.fromList . getWords $ s where
    len = B.length s
    s' = case len `mod` 2 of
            1 -> warn msg $ B.init s
            0 -> s
    msg = "Warning: truncating last byte of "++show len++" byte hex string before\n"
       ++ "         dumping. Dump works on 16-bit words only."
    getWords = either err id . runGet (getListOf getWord16be) where
        err e = error $ "Couldn't parse 16 bit words from even-length bytestring: "++e

