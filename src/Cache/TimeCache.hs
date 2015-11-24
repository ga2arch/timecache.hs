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
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as H
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool)

runTimeCache :: IO ()
runTimeCache = do
    mhook <- newMVar Nothing

    pool  <- runNoLoggingT $ createSqlitePool "timecache.sql" 5
    runDb pool $ runMigration migrateTables

    loadHook        mhook pool
    evictOldEntries mhook pool
    startWorker     mhook pool
    httpServer      mhook pool
