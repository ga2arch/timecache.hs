{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Server
    ( httpServer
    ) where

import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Database.Esqueleto
import           Network.HTTP.Types.Status (status200)
import qualified Web.Scotty                as SC

httpServer mh mhook pool = SC.scotty 8080 $ do
    SC.post "/" $ do
        TimeEntry value time <- SC.jsonData :: SC.ActionM TimeEntry
        liftIO $ do
            runDb pool $ insert $ TimeEntry value time
            insertEntry mh time value
        SC.status status200

    SC.get "/setWebhook" $ do
        hook <- SC.param "hook"
        runDb pool $ do
            delete $ from $ \(t :: SqlExpr (Entity Webhook)) -> return ()
            insert $ Webhook hook
            liftIO $ swapMVar mhook $ Just $ Webhook hook
        SC.status status200
