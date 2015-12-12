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
    , getChan
    , getStart
    )where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.HashTable.IO            as H
import           Data.Text                    (Text, unpack)
import           GHC.IO.Handle
import qualified STMContainers.Map            as M
import qualified STMContainers.Set            as S

type HashTable k v = H.CuckooHashTable k v
type Key       = ByteString
type Value     = ByteString
type Timestamp = Int
type KVStore   = M.Map Key TimeEntry
type Buckets   = M.Map Timestamp (S.Set Key)

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
 ,   kvStore :: TVar KVStore
 ,   buckets :: TVar Buckets
 ,   chan    :: TChan Action
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

getBuckets :: TimeCache (TVar Buckets)
getBuckets = gets buckets

getKVStore :: TimeCache (TVar KVStore)
getKVStore = gets kvStore

getChan :: TimeCache (TChan Action)
getChan = gets chan

getStart :: TimeCache Timestamp
getStart = gets start
