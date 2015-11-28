{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Cache.TimeCache.Types
    ( TimeCacheConfig(..)
    , TimeCacheState(..)
    , TimeCache(..)
    , KVStore
    , Timestamp

    , runT
    , getPool
    , getHook
    , getPort
    , getBuckets
    , getKVStore
    , getStart
    )where

import           Cache.TimeCache.Model
import           Control.Concurrent.STM.TVar
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import qualified Data.HashMap.Strict         as H
import           Data.IORef
import           Data.Pool                   (Pool)
import           Data.Text                   (Text, unpack)
import           Database.Esqueleto

type Timestamp = Int
type KVStore   = H.HashMap Text (TVar TimeEntry)
type Buckets   = H.HashMap Timestamp (TVar KVStore)

data TimeCacheConfig = TimeCacheConfig {
    db   :: Text
,   port :: Int
,   hook :: Text
}

data TimeCacheState = TimeCacheState {
    pool    :: Pool SqlBackend
,   start   :: Timestamp
,   kvStore :: TVar KVStore
,   buckets :: TVar Buckets
}

newtype TimeCache a = T { unT :: ReaderT TimeCacheConfig (StateT TimeCacheState IO) a}
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadReader TimeCacheConfig, MonadBase IO,
              MonadState TimeCacheState, MonadIO)

instance MonadBaseControl IO TimeCache where
    type StM TimeCache a = (a, TimeCacheState)
    liftBaseWith f = T . liftBaseWith $ \run -> f (run . unT)
    restoreM       = T . restoreM

runT :: TimeCacheConfig -> TimeCacheState -> TimeCache a -> IO a
runT config state f = evalStateT (runReaderT (unT f) config) state

getPool :: TimeCache (Pool SqlBackend)
getPool = gets pool

getHook :: TimeCache Text
getHook = asks hook

getPort :: TimeCache Int
getPort = asks port

getBuckets :: TimeCache (TVar Buckets)
getBuckets = gets buckets

getKVStore :: TimeCache (TVar KVStore)
getKVStore = gets kvStore

getStart :: TimeCache Timestamp
getStart = gets start
