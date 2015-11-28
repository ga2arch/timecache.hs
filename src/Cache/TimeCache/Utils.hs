{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Utils
    ( runDb
    , evictEntry
    , cacheEntry
    , storeEntry
    ) where

import           Cache.TimeCache.Model
import           Cache.TimeCache.Types
import Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import qualified Control.Exception            as E
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Either
import qualified Data.HashMap.Strict          as H
import           Data.Aeson
import           Data.Pool
import           Data.String.Conversions
import           Data.Text                    (Text, unpack)
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool, runSqlPool)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import GHC.Conc.Sync

runDb :: SqlPersistT (NoLoggingT (ResourceT TimeCache)) a -> TimeCache a
runDb f = do
    pool <- getPool
    runResourceT $ runNoLoggingT $ runSqlPool f pool

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
    mkv <- getKVStore

    liftIO . atomically $
        modifyTVar' mkv $ \kv -> H.delete key kv

    runDb $ delete $ from $ \t ->
        where_ (t ^. TimeEntryKey ==. val key)

    resp <- post value

    if isRight resp
        then liftIO $ putStrLn " OK."
        else liftIO $ putStrLn " Fail."

cacheEntry :: TimeEntry -> TimeCache ()
cacheEntry entry@(TimeEntry key value time) = do
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

storeEntry :: TimeEntry -> TimeCache ()
storeEntry entry@(TimeEntry key value time) = runDb $ do
    e <- getEntry key
    case e of
        Just _ -> do
            liftIO $ putStrLn $ "Updating:  " ++ show entry
            updateEntry entry

        Nothing -> do
            liftIO $ putStrLn $ "Inserting:  " ++ show entry
            insertEntry entry
