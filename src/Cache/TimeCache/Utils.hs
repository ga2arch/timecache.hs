{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache.Utils
    ( insertEntry
    , restoreEntries
    , loadHook
    , runDb
    , send
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

insertEntry mh (TimeEntry value time) = modifyMVar_ mh $ \h -> do
    let bucket = H.lookupDefault [] time h
    return $ H.insert time (value:bucket) h

runDb pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool

restoreEntries mh pool mhook = do
    entries <- runDb pool $ select $ from return
    mapM_ f entries
  where
    f (entityVal -> entry) = do
        now <- round <$> getPOSIXTime

        when (timeEntryTimestamp entry >= now) $
            insertEntry mh entry

loadHook mhook pool = do
    hook <- runDb pool $ select $ from $
        \(t :: SqlExpr (Entity Webhook)) -> return t

    when (not $ null hook) $
        void $ swapMVar mhook $ Just . entityVal . head $ hook

send url value = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseUrl $ unpack url

    let request = initialRequest {
        method = "POST"
    ,   requestBody = RequestBodyLBS $ cs value
    }

    putStrLn $ "Evicting: " ++ (cs value)
    response <- httpLbs request manager
    putStrLn $ "The status code was: "
        ++ show (statusCode $ responseStatus response)
