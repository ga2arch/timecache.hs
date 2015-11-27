{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cache.TimeCache.Types where

import           Cache.TimeCache.Model
import           Control.Monad.Catch
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Data.Pool                 (Pool)
import           Data.Text                 (Text, unpack)
import           Database.Esqueleto

data TimeCacheConfig = TimeCacheConfig {
    db   :: Text
,   port :: Int
,   hook :: Text
}

data TimeCacheState = TimeCacheState {
    pool :: Pool SqlBackend
}

newtype TimeCache a = T { unT :: ReaderT TimeCacheConfig (StateT TimeCacheState IO) a}
    deriving (Functor, Applicative, Monad,
              MonadReader TimeCacheConfig, MonadState TimeCacheState,
              MonadIO, MonadCatch, MonadThrow, MonadMask)

getPool :: TimeCache (Pool SqlBackend)
getPool = gets pool

getHook :: TimeCache Text
getHook = asks hook
