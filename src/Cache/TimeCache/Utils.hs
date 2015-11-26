{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache.Utils
    ( evictOldEntries
    , runDb
    , send
    ) where

import           Cache.TimeCache.Types
import           Control.Concurrent.MVar
import qualified Control.Exception            as E
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as H
import           Data.String.Conversions
import           Data.Text                    (Text, unpack)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool, runSqlPool)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

runDb pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool

evictOldEntries :: TimeCache ()
evictOldEntries = do
    TimeCacheConfig url pool <- ask

    entries <- liftIO $ runDb pool $ select $ from return
    liftIO $ mapM_ (f url pool) entries
  where
    f url pool (entityVal -> entry@(TimeEntry key value time)) = do
        now <- round <$>  getPOSIXTime

        when (time <= now) $ do
            send url value
            runDb pool $ delete $ from $ \t ->
                    where_ (t ^. TimeEntryTimestamp ==. val time)

send url value = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseUrl $ unpack url

    let request = initialRequest {
        method = "POST"
    ,   requestBody = RequestBodyLBS $ cs value
    }

    putStr $ "Evicting: " ++ (cs value) ++ " ... "
    response <- E.try $ httpLbs request manager

    case response of
        Right _ -> putStrLn " Ok."
        Left (ex :: HttpException) -> putStrLn " Fail."
