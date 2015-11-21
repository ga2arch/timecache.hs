{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Aeson                   (FromJSON, ToJSON, encode)
import           Data.ByteString              (ByteString)
import           Data.Hashable                (Hashable)
import           Data.Monoid
import           Data.Pool                    (Pool)
import           Data.Streaming.Network
import           Data.String.Conversions
import           Data.Text                    (Text, unpack)
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Network.HTTP.Client

import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           Database.Persist             (selectList)
import           Database.Persist.Sqlite      (createSqlitePool, runSqlPool)
import           Database.Persist.TH
import           GHC.Generics                 (Generic)
import           Network.HTTP.Types.Status

import qualified Data.ByteString.Char8        as C
import qualified Data.HashMap.Strict          as H
import qualified Web.Scotty                   as SC

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Webhook
    endpoint Text
    deriving Show

TimeEntry
   value     Text
   timestamp Int

   deriving Show
   deriving Generic
|]

instance ToJSON   TimeEntry
instance FromJSON TimeEntry

insertEntry mh next value = modifyMVar_ mh $ \h -> do
    let bucket = H.lookupDefault [] next h
    return $ H.insert next (value:bucket) h

strategy :: MVar (H.HashMap Int [Text])
        -> Pool SqlBackend
        -> Text
        -> Int
        -> IO ()
strategy mh pool value time = do
    current <- round <$> getPOSIXTime
    let next = current + time
    runDb pool $ insert $ TimeEntry value next

    insertEntry mh next value

worker :: MVar (H.HashMap Int [Text])
        -> Pool SqlBackend
        -> MVar (Maybe Webhook)
        -> IO ()
worker mh pool mhook = do
    now <- round <$> getPOSIXTime
    mapM_ handle $ iterate (+1) now

  where
    handle current = do
        modifyMVar_ mh $ \h ->
            maybe (return h)
                  (f h current)
                  (H.lookup current h)

        threadDelay $ 1 * 10^6

    f h current bucket = do
        async $ do
            mapM_ process bucket

            runDb pool $ delete $ from $ \t ->
                where_ (t ^. TimeEntryTimestamp ==. val current)

        return $ H.delete current h

    process value = do
        hook <- readMVar mhook
        case hook of
            Just (Webhook url) -> send url value
            Nothing            -> return ()

    send url value = do
        manager <- newManager defaultManagerSettings
        initialRequest <- parseUrl $ unpack url

        let request = initialRequest {
            method = "POST"
        ,   requestBody = RequestBodyLBS $ cs value
        }

        response <- httpLbs request manager
        putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)

httpServer h mhook pool strategy = SC.scotty 8080 $ do
    SC.post "/" $ do
        TimeEntry value time <- SC.jsonData :: SC.ActionM TimeEntry
        liftIO $ strategy h pool value time
        SC.status status200

    SC.get "/setWebhook" $ do
        hook <- SC.param "hook"
        runDb pool $ do
            delete $ from $ \(t :: SqlExpr (Entity Webhook)) -> return ()
            insert $ Webhook hook
            liftIO $ swapMVar mhook $ Just $ Webhook hook
        SC.status status200

runDb pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool

restoreEntries h pool = do
    entries <- runDb pool $ select $ from return
    mapM_ (\x -> do
        now <- round <$> getPOSIXTime

        let time  = timeEntryTimestamp $ entityVal x
            value = timeEntryValue     $ entityVal x

        when (time >= now) $ insertEntry h time value) entries

startWorker mh mhook pool = do
    runDb pool $ runMigration migrateTables
    hook <- runDb pool $ select $ from $
        \(t :: SqlExpr (Entity Webhook)) -> return t

    if null hook
        then putMVar mhook Nothing
        else putMVar mhook $ Just $ entityVal . head $ hook

    restoreEntries mh pool
    async $ worker mh pool mhook

main :: IO ()
main = do
    mh <- newMVar H.empty
    mhook <- newEmptyMVar

    pool <- runNoLoggingT $ createSqlitePool "timecache.sql" 5

    liftIO $ startWorker mh mhook pool
    httpServer mh mhook pool strategy
