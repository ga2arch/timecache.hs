{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache.Utils
    ( insertEntry
    , loadEntries
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

insertEntry mh (TimeEntry value time) = modifyMVar_ mh $ \h -> do
    let bucket = H.lookupDefault [] time h
    return $ H.insert time (value:bucket) h

runDb pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool

loadEntries mh pool mhook = do
    entries <- runDb pool $ select $ from return
    mapM_ f entries
  where
    f (entityVal -> entry@(TimeEntry value time)) = do
        now <- round <$> getPOSIXTime

        if time >= now
            then insertEntry mh entry
            else do
                awhenM (readMVar mhook) $
                    \(Webhook url) -> send url value

                runDb pool $ delete $ from $ \t ->
                    where_ (t ^. TimeEntryTimestamp ==. val time)

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

    putStr $ "Evicting: " ++ (cs value) ++ " ... "
    response <- E.try $ httpLbs request manager

    case response of
        Right _ -> putStrLn " Ok."
        Left (ex :: HttpException) -> putStrLn " Fail."
