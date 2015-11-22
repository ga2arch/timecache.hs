{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.HashMap.Strict      as H
import           Data.Pool                (Pool)
import           Data.Text                (Text, unpack)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           System.Posix.Unistd

worker :: MVar (H.HashMap Int [Text])
        -> Pool SqlBackend
        -> MVar (Maybe Webhook)
        -> IO ()
worker mh pool mhook = do
    now <- round <$> getPOSIXTime
    mapM_ handle $ iterate (+1) now

  where
    handle current = do
        print $ show current
        async $ modifyMVar_ mh $ \h ->
            maybe (return h)
                  (f h current)
                  (H.lookup current h)

        usleep $ 1 * 10^6

    f h current bucket = do
        mapM_ process bucket

        runDb pool $ delete $ from $ \t ->
            where_ (t ^. TimeEntryTimestamp ==. val current)

        return $ H.delete current h

    process value =
        readMVar mhook >>= \case
            Just (Webhook url) -> send url value
            Nothing            -> return ()

startWorker mh mhook pool = do
    runDb pool $ runMigration migrateTables

    restoreEntries mh pool mhook
    async $ worker mh pool mhook
