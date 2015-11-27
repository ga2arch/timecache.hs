{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Server
    ( httpServer
    ) where

import           Cache.TimeCache.Model
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

httpServer pool port = SC.scotty port $ do
    SC.post "/insert" $ do
        entry <- SC.jsonData :: SC.ActionM TimeEntry
        liftIO $ do
            putStrLn $ "Inserting: " ++ show entry
            runDb pool $ insert entry

        SC.status status200

    SC.post "/update" $ do
        TimeEntry key value time <- SC.jsonData :: SC.ActionM TimeEntry
        liftIO $ do
            putStrLn $ "Updating: " ++ show key
            runDb pool $
                update $ \t -> do
                    set t [TimeEntryValue     =. val value,
                           TimeEntryTimestamp =. val time]
                    where_ (t ^. TimeEntryKey ==. val key)
