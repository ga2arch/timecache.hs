{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache
    ( runTimeCache
    ) where

import           Cache.TimeCache.Server
import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Cache.TimeCache.Worker
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as H
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool)

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config@(TimeCacheConfig url pool) = do
    (flip runReaderT) config $ unT $ do
        evictOldEntries
        worker

    liftIO $ httpServer pool
