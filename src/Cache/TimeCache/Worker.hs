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

        liftIO $ do
            buckets <- takeMVar mbuckets

            res <- H.lookup buckets now
            case res of
                Just bucket -> do
                    H.delete buckets now
                    putMVar mbuckets buckets

                    void . liftIO . async $
                        H.mapM_ (runT config state . evictKey . fst) bucket

                Nothing -> do
                    putMVar mbuckets buckets

        nnow <- liftIO $ do
            threadDelay $ interval*10^6
            round <$> getPOSIXTime

        --liftIO $ print $ show nnow
        handle now nnow

    evictKey :: Text -> TimeCache ()
    evictKey key = do
        entry <- getEntry key

        case entry of
            Just e  -> evictEntry e
            Nothing -> return ()
