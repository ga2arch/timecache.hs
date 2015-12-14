{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Utils where

import           Cache.TimeCache.Binary
import           Cache.TimeCache.Types
import           Control.Concurrent.STM.TBChan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import qualified Control.Exception          as E
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Either
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, unpack)
import qualified ListT                      as LT
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import qualified Data.HashMap.Strict              as M
import qualified Data.HashSet          as S

post :: Value -> TimeCache (Either HttpException Bool)
post value = do
    hook <- getHook

    manager <- liftIO $ newManager defaultManagerSettings
    initialRequest <- parseUrl $ unpack hook

    let request = initialRequest {
        method = "POST"
    ,   requestBody = RequestBodyLBS $ BL.fromStrict value
    }

    response <- liftIO $ E.try $ httpLbs request manager
    case response of
        Right _                    -> return $ Right True
        Left (ex :: HttpException) -> return $ Left ex

evictEntry :: TimeEntry -> TimeCache ()
evictEntry e@(TimeEntry key value _) = do
    liftIO $ putStr $ "Evicting: " ++ (C.unpack value) ++ " ... "
    deleteEntry key

    resp <- post value
    if isRight resp
        then liftIO $ putStrLn " OK."
        else liftIO $ putStrLn " Fail."

cacheEntry :: TimeEntry -> TimeCache ()
cacheEntry entry@(TimeEntry key value time) = do
    liftIO $ putStrLn $ "Caching: " ++ show entry

    start    <- getStart
    mkvStore <- getKVStore
    mbuckets <- getBuckets
    interval <- getInterval

    let diff    = time - start
        offset  = (ceiling $
            (fromIntegral diff) / (fromIntegral interval)) * interval
        newTime = start + offset

    let correctedEntry = entry {
        timeEntryTimestamp = newTime
    }

    liftIO $ atomically $ do
        kvStore <- readTVar mkvStore

        case M.lookup key kvStore of
            Just entry -> do
                writeTVar mkvStore $! M.insert key correctedEntry kvStore

                let oldTime = timeEntryTimestamp entry
                when (oldTime /= newTime) $ do
                    moveBucket mbuckets key oldTime newTime

            Nothing -> do
                writeTVar mkvStore $! M.insert key correctedEntry kvStore
                insertIntoBucket mbuckets key newTime
    return ()
  where
    moveBucket :: TVar Buckets -> Key -> Timestamp -> Timestamp -> STM ()
    moveBucket mbuckets key oldTime newTime = do
        buckets <- readTVar mbuckets
        case M.lookup oldTime buckets of
            Just bucket -> do
                modifyTVar' bucket $ S.delete key
            Nothing      -> return ()

        insertIntoBucket mbuckets key newTime

    insertIntoBucket :: TVar Buckets -> Key -> Timestamp -> STM ()
    insertIntoBucket mbuckets key time = do
        buckets <- readTVar mbuckets
        case M.lookup time buckets of
            Just bucket -> modifyTVar' bucket $ S.insert key

            Nothing -> do
                bucket <- newTVar (S.fromList [key])
                modifyTVar' mbuckets $ M.insert time bucket

appendLog :: Action -> TimeCache ()
appendLog action = do
    chan <- getChan
    liftIO $ atomically $ writeTBChan chan action

insertEntry :: TimeEntry -> TimeCache ()
insertEntry entry = do
    cacheEntry entry
    appendLog $ Insert entry

deleteEntry :: Key -> TimeCache ()
deleteEntry key = do
    liftIO $ putStrLn $ "Deleting key: " ++ show key
    deleteKey key
    appendLog $ Delete key

deleteEntries :: Key -> TimeCache ()
deleteEntries prefix = do
    mkvStore <- getKVStore
    chan     <- getChan

    liftIO $ atomically $ do
       kvStore <- readTVar mkvStore
       let entries = M.toList kvStore
       mapM_ (f mkvStore chan) entries
  where
    f mkvStore chan (k, v) = when (C.isPrefixOf prefix k) $ do
        --liftIO $ putStrLn $ "Deleting key: " ++ show k
        modifyTVar' mkvStore $ M.delete k
        writeTBChan chan $ Delete k

getEntry :: Key -> TimeCache (Maybe TimeEntry)
getEntry key = do
    mkvStore <- getKVStore
    liftIO $ atomically $ do
        kvStore <- readTVar mkvStore
        case M.lookup key kvStore of
            Just me -> return $ Just me
            Nothing -> return Nothing

deleteKey :: Key -> TimeCache ()
deleteKey key = do
    mkvStore <- getKVStore
    liftIO $ atomically $ do
        modifyTVar' mkvStore $ M.delete key
