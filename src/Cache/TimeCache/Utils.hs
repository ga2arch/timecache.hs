{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Utils where

import           Cache.TimeCache.Binary
import           Cache.TimeCache.Types
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
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
import qualified Data.HashTable.IO          as H
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, unpack)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

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

    liftIO $ do
        kvStore <- takeMVar mkvStore
        buckets <- takeMVar mbuckets

        res <- H.lookup kvStore key
        case res of
            Just entry -> do
                H.insert kvStore key correctedEntry
                let oldTime = timeEntryTimestamp entry
                when (oldTime /= newTime) $ do
                    moveBucket buckets key oldTime newTime

            Nothing -> do
                H.insert kvStore key correctedEntry
                insertIntoBucket buckets key newTime

        putMVar mbuckets buckets
        putMVar mkvStore kvStore

    return ()
  where
    moveBucket :: Buckets -> Key -> Timestamp -> Timestamp -> IO ()
    moveBucket buckets key oldTime newTime = do
        res <- H.lookup buckets oldTime
        case res of
            Just bucket -> H.delete bucket key
            Nothing      -> return ()

        insertIntoBucket buckets key newTime

    insertIntoBucket :: Buckets -> Key -> Timestamp -> IO ()
    insertIntoBucket buckets key time = do
        res <- H.lookup buckets time
        case res of
            Just bucket -> H.insert bucket key ()
            Nothing -> do
                bucket <- H.fromList [(key, ())]
                H.insert buckets time bucket

appendLog :: Action -> TimeCache ()
appendLog action = do
    chan <- getChan
    liftIO $ writeChan chan action

insertEntry :: TimeEntry -> TimeCache ()
insertEntry entry = do
    cacheEntry entry
    appendLog $ Insert entry

deleteEntry :: Key -> TimeCache ()
deleteEntry key = do
    liftIO $ putStrLn $ "Deleting key: " ++ show key
    mkvStore <- getKVStore
    liftIO $ do
        kvStore <- takeMVar mkvStore
        H.delete kvStore key
        putMVar mkvStore kvStore

    appendLog $ Delete key

getEntry :: Key -> TimeCache (Maybe TimeEntry)
getEntry key = do
    mkvStore <- getKVStore
    liftIO $ do
        kvStore <- readMVar mkvStore
        res <- H.lookup kvStore key
        case res of
            Just me -> return $ Just me
            Nothing -> return Nothing

deleteKeyFromStore :: Key -> TimeCache ()
deleteKeyFromStore key = do
    mkvStore <- getKVStore
    liftIO $ do
        kvStore <- takeMVar mkvStore
        H.delete kvStore key
        putMVar mkvStore kvStore
