{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache.Worker
    ( startWorker
    , loadHook
    ) where

import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Pool                (Pool)
import           Data.Text                (Text, unpack)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           System.Posix.Unistd

worker :: Pool SqlBackend -> MVar (Maybe Webhook) -> IO ()
worker pool mhook = do
    since <- round <$> getPOSIXTime
    handle since (since-1) since

  where
    handle since before now = do
        async $ do
            entries <- runDb pool $ select $ from $ \t -> do
                where_ (t ^. TimeEntryTimestamp <=. val now)
                where_ (t ^. TimeEntryTimestamp >=. val before)
                return t

            mapM_ process entries

        usleep $ 1 * 10^6
        if (now - since > 60)
            then do
                since <- round <$> getPOSIXTime
                handle since (since-1) since
            else handle since now (now + 1)

    process (entityVal -> TimeEntry _ value time) =
        awhenM (readMVar mhook) $
            \(Webhook url) -> do
                send url value
                runDb pool $ delete $ from $ \t ->
                    where_ (t ^. TimeEntryTimestamp ==. val time)

startWorker mhook pool = async $ worker pool mhook
