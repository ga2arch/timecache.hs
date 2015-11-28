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
import qualified Data.HashTable.IO as H
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
        res <- unsafeIOToSTM $ H.lookup mkvStore key

        case res of
            Just me -> do
                entry <- readTVar me
                writeTVar me correctedEntry
                let oldTime = timeEntryTimestamp entry
                when (oldTime /= newTime) $ do
                    moveBucket mbuckets key oldTime newTime

            Nothing -> do
                me <- newTVar correctedEntry
                unsafeIOToSTM $ H.insert mkvStore key me
                insertIntoBucket mbuckets key newTime

    return ()
  where
    moveBucket :: Buckets -> Text -> Timestamp -> Timestamp -> STM ()
    moveBucket mbuckets key oldTime newTime = do
        res <- unsafeIOToSTM $ H.lookup mbuckets oldTime
        case res of
            Just mbucket ->
                unsafeIOToSTM $ H.delete mbucket key

            Nothing -> return ()

        insertIntoBucket mbuckets key newTime

    insertIntoBucket :: Buckets -> Text -> Timestamp -> STM ()
    insertIntoBucket mbuckets key time = do
        res <- unsafeIOToSTM $ H.lookup mbuckets time
        case res of
            Just mbucket ->
                unsafeIOToSTM $ H.insert mbucket key ()

            Nothing -> do
                mbucket <- unsafeIOToSTM $ H.fromList [(key, ())]
                unsafeIOToSTM $ H.insert mbuckets time mbucket

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
    liftIO . atomically $
        unsafeIOToSTM $ H.delete mkvStore key

    appendLog $ Delete key

getEntry :: Text -> TimeCache (Maybe TimeEntry)
getEntry key = do
    mkvStore <- getKVStore
    liftIO . atomically $ do
        res <- unsafeIOToSTM $ H.lookup mkvStore key
        case res of
            Just me -> readTVar me >>= return . Just
            Nothing -> return Nothing

deleteKeyFromStore :: Text -> TimeCache ()
deleteKeyFromStore key = do
    mkvStore <- getKVStore
    liftIO . atomically $
        unsafeIOToSTM $ H.delete mkvStore key
