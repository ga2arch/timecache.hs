module Cache.TimeCache.Binary where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.ByteString
import           Blaze.ByteString.Builder.Int
import           Blaze.ByteString.Builder.Word
import           Cache.TimeCache.Types
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8 as C
import           Data.Monoid
import           Data.Word
import           Prelude                             hiding (take)
import Data.Bits

-- |size|key|size|value|time|
serializeEntry (TimeEntry key value time) = do
    let keySize   = sizeToWords $ B.length key
    let valueSize = sizeToWords $ B.length value
    let b = fromWrite $ writeSize keySize
            <> writeByteString key
            <> writeSize valueSize 
            <> writeByteString value
            <> writeWord64be (fromIntegral time)
    toByteString b

writeSize (x, 0, 0, 0) = writeWord8 x
writeSize (x, a, b, 0) = writeWord8 x
                         <> writeWord8 a
                         <> writeWord8 b
writeSize (x, a, b, c) = writeWord8 x
                       <> writeWord8 a
                       <> writeWord8 b
                       <> writeWord16le c

sizeToWords :: Int -> (Word8, Word8, Word8, Word16)
sizeToWords x | x < 255 = (fromIntegral x, 0, 0, 0)
              | x > 254 && x < 65536 =
              (254,
               fromIntegral $ x .&. 0xFF,
               fromIntegral $ (x .&. 0xFFFF) `shiftR` 8, 0)
              | otherwise =
              (255,
               fromIntegral $ x .&. 0xFF,
               fromIntegral $ (x .&. 0xFFFF) `shiftR` 8, fromIntegral $ x `shiftR` 16)

words8toWord16 :: Word8 -> Word8 -> Word16
words8toWord16 a b = (fromIntegral  b) `shiftL` 8 .|. (fromIntegral a)

serializeAction (Insert entry) = do
    let e = serializeEntry entry
    let b = fromWrite $ writeWord8 0
            <> writeByteString e
    toByteString b

serializeAction (Delete key) = do
    let size = sizeToWords $ B.length key
    let b = fromWrite $ writeWord8 1
            <> writeSize size
            <> writeByteString key 
    toByteString b

deserialize = parseOnly (many' actionParser)

parseSize = do
    header <- anyWord8
    case fromIntegral header of
        254 -> do
            s1 <- anyWord8
            s2 <- anyWord8
            return $ fromIntegral $ words8toWord16 s1 s2

        255 -> do
            s1 <- anyWord8
            s2 <- anyWord8
            s3 <- anyWord16le

            let half = words8toWord16 s1 s2
            let size = ((fromIntegral s3) `shiftL` 16 .|. (fromIntegral half)) :: Int
            return $ fromIntegral size

        size -> return size

insertParser = do
    keySize   <- parseSize 
    key       <- take $ fromIntegral keySize
    valueSize <- parseSize
    value     <- take $ fromIntegral valueSize
    timestamp <- anyWord64be
    return $ Insert $ TimeEntry key value $ fromIntegral timestamp

deleteParser = do
    keySize <- parseSize
    key     <- take $ fromIntegral keySize
    return $ Delete key

actionParser = do
    action <- anyWord8
    case fromIntegral action of
        0 -> insertParser
        1 -> deleteParser
