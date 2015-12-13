{-# LANGUAGE OverloadedStrings #-}
module Cache.TimeCache.Binary
       ( serializeAction
       , deserialize
       ) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.ByteString
import           Blaze.ByteString.Builder.Int
import           Blaze.ByteString.Builder.Word
import           Cache.TimeCache.Types
import           Control.Applicative                 ((<|>))
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.Bits
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as C
import           Data.Monoid
import           Data.Word
import           Prelude                             hiding (take)
import qualified Data.Sequence as S

-- |size|key|size|value|time|
serializeEntry (TimeEntry key value time) = do
    let keySize   = sizeToWords $ B.length key
    let valueSize = sizeToWords $ B.length value
    writeToByteString $ writeSize keySize
            <> writeByteString key
            <> writeSize valueSize
            <> writeByteString value
            <> writeWord64be (fromIntegral time)

writeSize (x, 0, 0, 0) = writeWord8 x
writeSize (x, a, b, 0) = mconcat [writeWord8 x, writeWord8 a, writeWord8 b]
writeSize (x, a, b, c) = mconcat [writeWord8 x, writeWord8 a,
                                  writeWord8 b, writeWord16le c]

sizeToWords :: Int -> (Word8, Word8, Word8, Word16)
sizeToWords x | x < 255 = (fromIntegral x, 0, 0, 0)
              | x > 254 && x < 65536 = (254, fromIntegral $ fstByte x,
                                             fromIntegral $ sndByte x, 0)
              | otherwise =  (255, fromIntegral $ fstByte x,
                                   fromIntegral $ sndByte x,
                                   fromIntegral $ lastBytes x)
  where
    fstByte x   = x .&. 0xFF
    sndByte x   = (x .&. 0xFFFF) `shiftR` 8
    lastBytes x = x `shiftR` 16

words8toWord16 :: Word8 -> Word8 -> Word16
words8toWord16 a b = (fromIntegral  b) `shiftL` 8 .|. (fromIntegral a)

serializeAction (Insert entry) = do
    let e = serializeEntry entry
    writeToByteString $ writeWord8 0 <> writeByteString e

serializeAction (Delete key) = do
    let size = sizeToWords $ B.length key
    writeToByteString $ writeWord8 1
            <> writeSize size
            <> writeByteString key

deserialize :: C.ByteString -> S.Seq Action
deserialize input = do
    go S.empty input
  where
    go actions "" = actions
    go actions input = do
        case parse actionParser input of
          Done r action -> go (actions S.|> action) r
          _ -> go actions ""

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
            return $! fromIntegral size

        size -> return size

insertParser = do
    word8 0
    keySize   <- parseSize
    key       <- take $ fromIntegral keySize
    valueSize <- parseSize
    value     <- take $ fromIntegral valueSize
    timestamp <- anyWord64be
    return $! Insert $ TimeEntry key value $ fromIntegral timestamp

deleteParser = do
    word8 1
    keySize <- parseSize
    key     <- take $ fromIntegral keySize
    return $! Delete key

actionParser = insertParser <|> deleteParser
