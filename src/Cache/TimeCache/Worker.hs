{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Worker
    ( worker
    ) where

import           Cache.TimeCache.Model
import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Reader
import           Data.Pool                (Pool)
import           Data.Text                (Text, unpack)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           System.Clock
import           System.Posix.Unistd

worker :: TimeCache ()
worker = do
    pool <- getPool
    hook <- getHook

    void . liftIO . async $ do
        now <- liftIO $ round <$> getPOSIXTime
        handle hook pool (now-1) now

  where
    handle hook pool before now = do
        async $ do
            entries <- runDb pool $ select $ from $ \t -> do
                where_ (t ^. TimeEntryTimestamp <=. val now)
                where_ (t ^. TimeEntryTimestamp >=. val before)
                return t

            mapM_ (evict hook pool . entityVal) entries

        threadDelay $ 1*10^6
        round <$> getPOSIXTime >>= handle hook pool now
