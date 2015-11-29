{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Cache.TimeCache.Types
    ( TimeEntry(..)
    , TimeCacheConfig(..)
    , TimeCacheState(..)
    , TimeCache(..)
    , Action(..)
    , KVStore
    , Buckets
    , Key
    , Value
    , Timestamp

    , runT
    , getHook
    , getPort
    , getInterval
    , getBuckets
    , getKVStore
    , getStart
    )where

import           Control.Concurrent.MVar
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.HashTable.IO           as H
import           Data.Text                   (Text, unpack)
import           GHC.IO.Handle

type HashTable k v = H.CuckooHashTable k v
type Key       = ByteString
type Value     = ByteString
type Timestamp = Int
type KVStore   = HashTable Key TimeEntry
type Buckets   = HashTable Timestamp (HashTable Key ())

data TimeEntry = TimeEntry {
    timeEntryKey       :: !Key
,   timeEntryValue     :: !Value
,   timeEntryTimestamp :: !Timestamp
} deriving (Show, Read)

data TimeCacheConfig = TimeCacheConfig {
    port     :: Int
,   hook     :: Text
,   interval :: Int
}

data TimeCacheState = TimeCacheState {
    start   :: Timestamp
,   kvStore :: MVar KVStore
,   buckets :: MVar Buckets
}

newtype TimeCache a = T { unT :: ReaderT TimeCacheConfig (StateT TimeCacheState IO) a}
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadReader TimeCacheConfig, MonadBase IO,
              MonadState TimeCacheState, MonadIO)

instance MonadBaseControl IO TimeCache where
    type StM TimeCache a = (a, TimeCacheState)
    liftBaseWith f = T . liftBaseWith $ \run -> f (run . unT)
    restoreM       = T . restoreM

data Action = Insert TimeEntry | Delete Key
    deriving (Show, Read)


runT :: TimeCacheConfig -> TimeCacheState -> TimeCache a -> IO a
runT config state f = evalStateT (runReaderT (unT f) config) state

getHook :: TimeCache Text
getHook = asks hook

getPort :: TimeCache Int
getPort = asks port

getInterval :: TimeCache Int
getInterval = asks interval

getBuckets :: TimeCache (MVar Buckets)
getBuckets = gets buckets

getKVStore :: TimeCache (MVar KVStore)
getKVStore = gets kvStore

getStart :: TimeCache Timestamp
getStart = gets start
