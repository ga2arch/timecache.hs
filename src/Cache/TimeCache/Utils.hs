{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Utils where

import           Cache.TimeCache.Types
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import qualified Control.Exception            as E
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict          as H
import           Data.String.Conversions
import           Data.Text                    (Text, unpack)
import           GHC.Conc.Sync
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

post :: Text -> TimeCache (Either HttpException Bool)
post value = do
    hook <- getHook

    manager <- liftIO $ newManager defaultManagerSettings
    initialRequest <- parseUrl $ unpack hook

    let request = initialRequest {
        method = "POST"
    ,   requestBody = RequestBodyLBS $ cs value
    }

    response <- liftIO $ E.try $ httpLbs request manager
    case response of
        Right _                    -> return $ Right True
        Left (ex :: HttpException) -> return $ Left ex

evictEntry :: TimeEntry -> TimeCache ()
evictEntry e@(TimeEntry key value _) = do
    liftIO $ putStr $ "Evicting: " ++ (unpack value) ++ " ... "
    deleteEntry key

    resp <- post value
    if isRight resp
        then liftIO $ putStrLn " OK."
        else liftIO $ putStrLn " Fail."

cacheEntry :: TimeEntry -> TimeCache ()
cacheEntry entry@(TimeEntry key value time) = do
    --liftIO $ putStrLn $ "Caching: " ++ show entry

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

    liftIO . atomically $ do
        kvStore <- readTVar mkvStore

        case H.lookup key kvStore of
            Just me -> do
                entry <- readTVar me
                writeTVar me correctedEntry
                let oldTime = timeEntryTimestamp entry
                when (oldTime /= newTime) $ do
                    moveBucket mbuckets me key oldTime newTime

            Nothing -> do
                me <- newTVar correctedEntry
                writeTVar mkvStore $ H.insert key me kvStore
                insertIntoBucket mbuckets me key newTime

    return ()
  where
    moveBucket mbuckets me key oldTime newTime = do
        buckets <- readTVar mbuckets

        case H.lookup oldTime buckets of
            Just mbucket -> do
                bucket <- readTVar mbucket
                writeTVar mbucket $ H.delete key bucket

            Nothing -> return ()

        insertIntoBucket mbuckets me key newTime

    insertIntoBucket mbuckets me key time = do
        buckets <- readTVar mbuckets

        case H.lookup time buckets of
            Just mbucket -> do
                bucket <- readTVar mbucket
                writeTVar mbucket $ H.insert key me bucket

            Nothing -> do
                mbucket <- newTVar $ H.fromList [(key, me)]
                writeTVar mbuckets $ H.insert time mbucket buckets

appendLog :: Action -> TimeCache ()
appendLog action = liftIO $
    appendFile "actions.log" $ show action ++ "\n"

insertEntry :: TimeEntry -> TimeCache ()
insertEntry entry = do
    cacheEntry entry
    appendLog $ Insert entry

deleteEntry :: Text -> TimeCache ()
deleteEntry key = do
    mkvStore <- getKVStore
    liftIO . atomically . modifyTVar' mkvStore $ H.delete key
    appendLog $ Delete key
