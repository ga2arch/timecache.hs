{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Worker
    ( worker
    ) where

import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict      as H
import           Data.IORef
import           Data.Text                (Text, unpack)
import           Data.Time.Clock.POSIX
import           System.Posix.Unistd

worker :: TimeCache ()
worker = do
    now <- liftIO $ round <$> getPOSIXTime
    handle now (now+1)

  where
    handle :: Timestamp -> Timestamp -> TimeCache ()
    handle before now = do
        config   <- ask
        state    <- get
        mbuckets <- getBuckets
        interval <- getInterval

        entries <- liftIO . atomically $ do
            buckets <- readTVar mbuckets

            case H.lookup now buckets of
                Just mbucket -> do
                    bucket <- readTVar mbucket
                    writeTVar mbuckets $ H.delete now buckets
                    return $ H.toList bucket

                Nothing -> writeTVar mbuckets buckets >> return []

        when (not . null $ entries) $
            void . liftIO . async . runT config state $
                mapM_  (evict . snd) entries

        nnow <- liftIO $ do
            threadDelay $ interval*10^6
            round <$> getPOSIXTime

        --liftIO $ print $ show nnow
        handle now nnow

    evict :: TVar TimeEntry -> TimeCache ()
    evict me = do
        mkvStore <- getKVStore
        entry <- liftIO . atomically $ do
            kvStore <- readTVar mkvStore
            TimeEntry key _ _ <- readTVar me
            case H.lookup key kvStore of
                Just e  -> readTVar e >>= return . Just
                Nothing -> return Nothing

        case entry of
            Just e  -> evictEntry e
            Nothing -> return ()
