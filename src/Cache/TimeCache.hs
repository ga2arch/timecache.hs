{-# LANGUAGE BangPatterns        #-}

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
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad                (when)
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.State          (evalStateT, get, put)
import           Control.Monad.Trans          (liftIO)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as C
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import qualified ListT                        as LT
import qualified STMContainers.Map            as M
import           System.Directory
import           System.IO
import           System.Posix
import           System.Posix.Signals

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

    f (Delete key) = deleteKey key

logger :: TVar KVStore -> String -> TChan Action -> IO ()
logger mkvStore filename chan = do
    (handle, size) <- hs filename
    go handle size (10^5)
  where
    go handle size maxSize
        | size >= maxSize = handleOverflow handle maxSize >>= appendAction
        | otherwise       = appendAction (handle, size, maxSize)

    handleOverflow handle maxSize = do
        putStrLn "Rebuilding log"
        hClose handle
        rebuildLog
        (handle, size) <- hs filename
        if size >= maxSize
           then return (handle, size, (maxSize * 2))
           else return (handle, size, 10^5)

    appendAction (handle, size, maxSize) = do
        !action  <- atomically $ readTChan chan
        let bdata = serializeAction action
        C.hPut handle bdata
        go handle (size + C.length bdata) maxSize

    rebuildLog  = do
        withFile "actions.temp" WriteMode $ \temp -> do
            hSetBuffering temp NoBuffering
            list  <- atomically $ do
                kvStore  <- readTVar mkvStore
                LT.toList $ M.stream kvStore
            mapM_ (write temp . snd) list
        renameFile "actions.temp" "actions.log"

    write h = C.hPut h . serializeAction . Insert

    hs :: String -> IO (Handle, Int)
    hs filename = do
        handle <- openFile filename AppendMode
        hSetBuffering handle NoBuffering
        size   <- fromIntegral.fileSize <$> getFileStatus filename
        return (handle, size)

runTimeCache :: TimeCacheConfig -> IO ()
runTimeCache config = do
    kvstore <- M.newIO >>= newTVarIO
    buckets <- M.newIO >>= newTVarIO
    chan    <- newTChanIO
    start   <- round <$> getPOSIXTime

    let state = TimeCacheState start kvstore buckets chan
    --installHandler sigTERM (Catch $ hClose handle) Nothing
    runT config state restoreEntries
    async $ runT config state worker
    async $ logger kvstore "actions.log" chan
    runT config state runHttpServer
