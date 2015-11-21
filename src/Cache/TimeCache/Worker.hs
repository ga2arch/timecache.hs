{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Cache.TimeCache.Worker
    ( startWorker
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

restoreEntries mh pool mhook = do
    entries <- runDb pool $ select $ from return
    mapM_ (\x -> do
        now <- round <$> getPOSIXTime

        let time  = timeEntryTimestamp $ entityVal x
            value = timeEntryValue     $ entityVal x

        if (time >= now)
            then insertEntry mh time value
            else tryReadMVar mhook >>= \case
                Just url -> send value url
                Nothing  -> return ()) entries

startWorker mh mhook pool = do
    runDb pool $ runMigration migrateTables
    hook <- runDb pool $ select $ from $
        \(t :: SqlExpr (Entity Webhook)) -> return t

    if null hook
        then putMVar mhook Nothing
        else putMVar mhook $ Just $ entityVal . head $ hook

    restoreEntries mh pool mhook
    async $ worker mh pool mhook
