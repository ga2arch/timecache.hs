{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache.Utils
    ( insertEntry
    , evictOldEntries
    , loadHook
    , runDb
    , send
    , awhenM
    ) where

import           Cache.TimeCache.Types
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Logger
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
import qualified Control.Exception as E

-- | Chainable anaphoric whenM.
awhenM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
awhenM test action =
    test >>= \case
        Just x  -> action x
        Nothing -> return ()

insertEntry mh (TimeEntry key value time) = modifyMVar_ mh $ \h -> do
    let bucket = H.lookupDefault [] time h
    return $ H.insert time (value:bucket) h

runDb pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool

evictOldEntries mhook pool = do
    entries <- runDb pool $ select $ from return
    mapM_ f entries
  where
    f (entityVal -> entry@(TimeEntry key value time)) = do
        now <- round <$> getPOSIXTime

        when (time <= now) $
            awhenM (readMVar mhook) $
                \(Webhook url) -> do
                    send url value
                    runDb pool $ delete $ from $ \t ->
                        where_ (t ^. TimeEntryTimestamp ==. val time)

loadHook mhook pool =
    swapMVar mhook $ Just "http://104.197.125.254:8000/expiration"

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
