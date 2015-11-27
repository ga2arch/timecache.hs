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
import           Control.Monad.State
import           Data.Pool                (Pool)
import           Data.Text                (Text, unpack)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto       hiding (get)
import           System.Clock
import           System.Posix.Unistd

worker :: TimeCache ()
worker = do
    now <- liftIO $ round <$> getPOSIXTime
    handle (now-1) now

  where
    handle before now = do
        config <- ask
        state  <- get

        liftIO . async . runT config state $ do
            entries <- runDb $ select $ from $ \t -> do
                where_ (t ^. TimeEntryTimestamp <=. val now)
                where_ (t ^. TimeEntryTimestamp >=. val before)
                return t

            mapM_ (evict . entityVal) entries

        nnow <- liftIO $ do
            threadDelay $ 1*10^6
            round <$> getPOSIXTime

        handle now nnow
