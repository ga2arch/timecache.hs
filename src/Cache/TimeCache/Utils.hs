{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Utils where

import           Cache.TimeCache.Binary
import           Cache.TimeCache.Types
import           Control.Concurrent.Chan
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
import qualified STMContainers.Map          as M hiding (Key, Value)
import qualified STMContainers.Set          as S

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
        buckets <- readTVar mbuckets

        res <- M.lookup key kvStore
        case res of
            Just entry -> do
                M.insert correctedEntry key kvStore

                let oldTime = timeEntryTimestamp entry
                when (oldTime /= newTime) $ do
                    moveBucket buckets key oldTime newTime

            Nothing -> do
                M.insert correctedEntry key kvStore
                insertIntoBucket buckets key newTime
    return ()
  where
    moveBucket :: Buckets -> Key -> Timestamp -> Timestamp -> STM ()
    moveBucket buckets key oldTime newTime = do
        res <- M.lookup oldTime buckets
        case res of
            Just bucket -> S.delete key bucket
            Nothing      -> return ()

        insertIntoBucket buckets key newTime

    insertIntoBucket :: Buckets -> Key -> Timestamp -> STM ()
    insertIntoBucket buckets key time = do
        res <- M.lookup time buckets
        case res of
            Just bucket -> S.insert key bucket
            Nothing -> do
                bucket <- S.new
                S.insert key bucket
                M.insert bucket time buckets

appendLog :: Action -> TimeCache ()
appendLog action = do
    chan <- getChan
    liftIO $ atomically $ writeTChan chan action

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
       entries <- LT.toList $ M.stream kvStore
       mapM_ (f kvStore chan) entries
  where
    f kvStore chan (k, v) = when (C.isPrefixOf prefix k) $ do
        --liftIO $ putStrLn $ "Deleting key: " ++ show k
        M.delete k kvStore
        writeTChan chan $ Delete k

getEntry :: Key -> TimeCache (Maybe TimeEntry)
getEntry key = do
    mkvStore <- getKVStore
    liftIO $ atomically $ do
        kvStore <- readTVar mkvStore
        res <- M.lookup key kvStore
        case res of
            Just me -> return $ Just me
            Nothing -> return Nothing

deleteKey :: Key -> TimeCache ()
deleteKey key = do
    mkvStore <- getKVStore
    liftIO $ atomically $ do
        kvStore <- readTVar mkvStore
        M.delete key kvStore
