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
import           Data.Monoid
import           Data.Word
import           Prelude                             hiding (take)

-- |size|key|size|value|time|
serializeEntry (TimeEntry key value time) = do
    let keySize   = B.length key
    let valueSize = B.length value
    let b = fromWrite $ writeInt32be (fromIntegral keySize)
            <> writeByteString key
            <> writeInt32be (fromIntegral valueSize)
            <> writeByteString value
            <> writeInt32be (fromIntegral time)

    toByteString b

serializeAction (Insert entry) = do
    let e = serializeEntry entry
    let b = fromWrite $ writeInt8 0
            <> writeByteString e
    toByteString b

serializeAction (Delete key) = do
    let b = fromWrite $ writeInt8 1
            <> writeByteString key 
    toByteString b

deserialize = parseOnly (many' actionParser)

insertParser = do
    keySize   <- anyWord32be
    key       <- take $ fromIntegral keySize
    valueSize <- anyWord32be
    value     <- take $ fromIntegral valueSize
    timestamp <- anyWord32be
    return $ Insert $ TimeEntry key value $ fromIntegral timestamp

deleteParser = do
    keySize <- anyWord32be
    key     <- take $ fromIntegral keySize
    return $ Delete key

actionParser = do
    action <- anyWord8
    case fromIntegral action of
        0 -> insertParser
        1 -> deleteParser
