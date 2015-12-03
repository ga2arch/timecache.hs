
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
import           Control.Concurrent.Chan
import           Control.Concurrent.Async
import           Control.Monad            (when)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.State      (evalStateT, get, put)
import           Control.Monad.Trans      (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8    as C
import qualified Data.HashTable.IO        as H
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           System.Directory
import           System.IO
import           System.Posix.Signals
import           System.Posix

restoreEntries :: TimeCache ()
restoreEntries = do
    exists <- liftIO $ doesFileExist "actions.log"
    when exists $ do
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

logger :: MVar KVStore -> String -> Chan Action -> IO ()
logger mkvStore filename chan = do
    handle <- openFile filename AppendMode
    size   <- fileSize <$> getFileStatus filename
    go handle size
  where
    go handle size =  
        if (size >= 10^6)
          then do
              putStrLn "Rebuilding log"
              hClose handle
              rebuildLog
              handle <- openFile filename AppendMode
              size   <- fileSize <$> getFileStatus filename
              go handle size
           else do
               action  <- readChan chan
               let bdata = serializeAction action
               C.hPut handle bdata
               go handle (size + (fromIntegral $ C.length bdata))

    rebuildLog  = do
        kvStore  <- readMVar mkvStore
        temp     <- openFile "actions.temp" WriteMode
        list     <- H.toList kvStore
        mapM_ (write temp . snd) list
        renameFile "actions.temp" "actions.log"

    write temp entry = 
        C.hPut temp $ serializeAction $ Insert entry

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config = do
    kvstore <- H.new >>= newMVar
    buckets <- H.new >>= newMVar
    chan    <- newChan
    start   <- round <$> getPOSIXTime

    let state = TimeCacheState start kvstore buckets chan
    --installHandler sigTERM (Catch $ hClose handle) Nothing
    runT config state restoreEntries
    async $ runT config state worker
    async $ logger kvstore "actions.log" chan
    runT config state runHttpServer
