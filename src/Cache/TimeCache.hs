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
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as H
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool)

evictOldEntries :: TimeCache ()
evictOldEntries = do
    pool <- getPool
    hook <- getHook

    entries <- liftIO $ runDb pool $ select $ from return
    liftIO $ mapM_ (f hook pool) entries
  where
    f hook pool (entityVal -> entry@(TimeEntry key value time)) = do
        now <- round <$> getPOSIXTime
        when (time <= now) $ evict hook pool entry

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config@(TimeCacheConfig db port hook) = do
    pool  <- liftIO $ runNoLoggingT $ createSqlitePool db 5
    liftIO $ runDb pool $ runMigration migrateTables

    evalStateT (runReaderT (unT f) config) (TimeCacheState pool)
    liftIO $ httpServer pool port
  where
   f = evictOldEntries >> worker
