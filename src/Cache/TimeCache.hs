
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache
    ( runTimeCache
    , TimeCacheConfig(..)
    ) where

import           Cache.TimeCache.Binary
import           Cache.TimeCache.Server
import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Cache.TimeCache.Worker
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad            (when)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.State      (evalStateT, get, put)
import           Control.Monad.Trans      (liftIO)
import qualified Data.ByteString.Char8    as C
import qualified Data.HashTable.IO        as H
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           System.Directory
import           System.IO
import           System.Posix.Signals

restoreEntries :: TimeCache ()
restoreEntries = do
    exists <- liftIO $ doesFileExist "actions.log"
    when (exists) $ do
        liftIO $ putStrLn "Restoring entries"
        content <- liftIO $ C.readFile "actions.log"
        case deserialize content of
            Left err      -> return ()
            Right actions -> mapM_ f actions

  where
    f (Insert entry) = do
        now <- liftIO $ round <$> getPOSIXTime
        when (timeEntryTimestamp entry >= now) $ cacheEntry entry

    f (Delete key) = deleteKeyFromStore key

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config = do
    kvstore <- H.new >>= newMVar
    buckets <- H.new >>= newMVar
    start   <- round <$> getPOSIXTime

    let state = TimeCacheState start kvstore buckets
    --installHandler sigTERM (Catch $ hClose handle) Nothing
    runT config state restoreEntries
    async $ runT config state worker
    runT config state runHttpServer
