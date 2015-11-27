{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Cache.TimeCache.Types where

import           Cache.TimeCache.Model
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Data.Pool                   (Pool)
import           Data.Text                   (Text, unpack)
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
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadReader TimeCacheConfig, MonadBase IO,
              MonadState TimeCacheState, MonadIO)

instance MonadBaseControl IO TimeCache where
    type StM TimeCache a = (a, TimeCacheState)
    liftBaseWith f = T (liftBaseWith (\run -> f (run . unT)))
    restoreM = T . restoreM

getPool :: TimeCache (Pool SqlBackend)
getPool = gets pool

getHook :: TimeCache Text
getHook = asks hook
