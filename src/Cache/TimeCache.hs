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
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad               (when)
import           Control.Monad.Logger        (runNoLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.State         (evalStateT)
import           Control.Monad.Trans         (liftIO)
import qualified Data.HashTable.IO           as H
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           GHC.Conc.Sync
import           System.Directory

restoreEntries :: TimeCache ()
restoreEntries = do
    exists  <- liftIO $ doesFileExist "actions.log"
    when (exists) $ do
        actions <- liftIO $ T.lines <$> TIO.readFile "actions.log"
        liftIO $ putStrLn "Restoring entries"
        mapM_ (f . read . T.unpack) actions
  where
    f (Insert entry) = do
        now <- liftIO $ round <$> getPOSIXTime
        when (timeEntryTimestamp entry >= now) $ cacheEntry entry

    f (Delete key) = deleteKeyFromStore key

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config@(TimeCacheConfig port hook interval) = do
    kvstore <- H.new >>= newMVar
    buckets <- H.new >>= newMVar
    start   <- round <$> getPOSIXTime

    let state = TimeCacheState start kvstore buckets

    runT config state restoreEntries
    async $ runT config state worker
    runT config state runHttpServer
