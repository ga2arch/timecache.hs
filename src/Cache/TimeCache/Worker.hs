module Cache.TimeCache.Worker
    ( worker
    ) where

import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.IORef
import           Data.Text                (Text, unpack)
import           Data.Time.Clock.POSIX
import           GHC.Conc.Sync
import qualified ListT                    as LT
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet        as S
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

        keys <- liftIO $ atomically $ do
              buckets <- readTVar mbuckets
              case M.lookup now buckets of
                  Just bucket -> do
                      writeTVar mbuckets $ M.delete now buckets
                      b <- readTVar bucket
                      return $ S.toList b
                  Nothing -> return []

        liftIO $ async $ runT config state $ mapM_ evictByKey keys

        nnow <- liftIO $ do
            threadDelay $ interval*10^6
            round <$> getPOSIXTime

        --liftIO $ print $ show nnow
        handle now nnow

    evictByKey :: Key -> TimeCache ()
    evictByKey key = do
       entry <- getEntry key

       case entry of
           Just e  -> evictEntry e
           Nothing -> return ()
