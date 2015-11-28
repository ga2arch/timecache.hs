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
import qualified Data.HashTable.IO as H
import           Data.IORef
import           Data.Text                (Text, unpack)
import           Data.Time.Clock.POSIX
import           System.Posix.Unistd
import GHC.Conc.Sync

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
            res <- unsafeIOToSTM $ H.lookup mbuckets now
            case res of
                Just mbucket -> do
                    unsafeIOToSTM $ H.delete mbuckets now
                    lt <- unsafeIOToSTM $ H.toList mbucket
                    return lt

                Nothing -> return []

        when (not . null $ entries) $
            void . liftIO . async . runT config state $
                mapM_  (evict . fst) entries

        nnow <- liftIO $ do
            threadDelay $ interval*10^6
            round <$> getPOSIXTime

        --liftIO $ print $ show nnow
        handle now nnow

    evict :: Text -> TimeCache ()
    evict key = do
        mkvStore <- getKVStore
        entry <- liftIO . atomically $ do
            res <- unsafeIOToSTM $ H.lookup mkvStore key
            case res of
                Just e  -> readTVar e >>= return . Just
                Nothing -> return Nothing

        case entry of
            Just e  -> evictEntry e
            Nothing -> return ()
