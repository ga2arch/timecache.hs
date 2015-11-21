{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Server
    ( httpServer
    ) where

import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           Network.HTTP.Types.Status (status200)

import qualified Web.Scotty                as SC

httpServer mh mhook pool = SC.scotty 8080 $ do
    SC.post "/" $ do
        TimeEntry value time <- SC.jsonData :: SC.ActionM TimeEntry
        liftIO $ do
            runDb pool $ insert $ TimeEntry value time
            --insertEntry mh time value
            async $ do
                now <- round <$> getPOSIXTime

                threadDelay $ (time - now) * 10^6
                tryReadMVar mhook >>= \case
                    Just url -> send url value
                    Nothing  -> return ()

        SC.status status200

    SC.get "/setWebhook" $ do
        hook <- SC.param "hook"
        runDb pool $ do
            delete $ from $ \(t :: SqlExpr (Entity Webhook)) -> return ()
            insert $ Webhook hook

        liftIO $ tryReadMVar mhook >>= \case
            Just _  -> void $ swapMVar mhook hook
            Nothing -> putMVar  mhook hook

        SC.status status200
