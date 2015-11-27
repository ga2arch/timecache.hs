{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache
    ( runTimeCache
    , TimeCacheConfig(..)
    ) where

import           Cache.TimeCache.Model
import           Cache.TimeCache.Server
import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Cache.TimeCache.Worker
import           Control.Concurrent.Async
import           Control.Monad                (when)
import           Control.Monad.Logger         (runNoLoggingT)
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.State          (evalStateT)
import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool)

evictOldEntries :: TimeCache ()
evictOldEntries = do
    entries <- runDb $ select $ from return
    mapM_ f entries
  where
    f (entityVal -> entry@(TimeEntry key value time)) = do
        now <- liftIO $ round <$> getPOSIXTime
        when (time <= now) $ evict entry

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config@(TimeCacheConfig db port hook) = do
    pool  <- runNoLoggingT $ createSqlitePool db 5
    runResourceT $ runNoLoggingT $ runSqlPool (runMigration migrateTables) pool

    let state = TimeCacheState pool
    async $ runT config state $ evictOldEntries >> worker
    runT config state runHttpServer
