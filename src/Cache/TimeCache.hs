{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache
    ( runTimeCache
    , TimeCacheConfig(..)
    ) where

import           Cache.TimeCache.Server
import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Cache.TimeCache.Worker
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad                (when)
import           Control.Monad.Logger         (runNoLoggingT)
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.State          (evalStateT)
import           Control.Monad.Trans          (liftIO)
import qualified Data.HashMap.Strict          as H
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           System.Directory

restoreEntries :: TimeCache ()
restoreEntries = do
    exists  <- liftIO $ doesFileExist "actions.log"
    when (exists) $ do
        !actions <- liftIO $ lines <$> readFile "actions.log"
        liftIO $ putStrLn "Restoring entries"
        mapM_ (f . read) actions
  where
    f (Insert entry@(TimeEntry key value time)) = do
        now <- liftIO $ round <$> getPOSIXTime
        when (time >= now) $ cacheEntry entry

    f (Delete key) = do
        mkvStore <- getKVStore
        liftIO . atomically . modifyTVar' mkvStore $ H.delete key

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config@(TimeCacheConfig port hook interval) = do
    kvstore <- newTVarIO H.empty
    buckets <- newTVarIO H.empty
    start   <- round <$> getPOSIXTime

    let state = TimeCacheState start kvstore buckets

    runT config state restoreEntries
    async $ runT config state worker
    runT config state runHttpServer
