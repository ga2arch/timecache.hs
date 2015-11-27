{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
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
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Maybe                   (listToMaybe)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           Network.HTTP.Types.Status
import qualified Web.Scotty                   as SC

httpServer pool port = SC.scotty port $ do
    SC.post "/insert" $ do
        entry@(TimeEntry key value time) <- SC.jsonData :: SC.ActionM TimeEntry
        liftIO $
            runQuery pool $ do
                e <- getEntry key
                case e of
                    Just _ -> do
                        liftIO $ putStrLn $ "Updating:  " ++ show key
                        updateEntry entry

                    Nothing -> do
                        liftIO $ putStrLn $ "Inserting:  " ++ show entry
                        insertEntry entry
        SC.status status200

    SC.delete "/delete/:key" $ do
        key <- SC.param "key"
        liftIO $ do
            putStrLn $ "Deleting: " ++ show key
            runQuery pool $ deleteEntry key
        SC.status status200

    SC.get "/entries/:key" $ do
        key <- SC.param "key"
        query <- liftIO $ runQuery pool $ getEntry key
        case query of
            Just (entityVal -> entry) -> SC.json entry
            Nothing                   -> SC.status status404

  where
    runQuery pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool
    insertEntry entry = void $ insert entry

    updateEntry (TimeEntry key value time) =
        update $ \t -> do
            set t [TimeEntryValue     =. val value,
                   TimeEntryTimestamp =. val time]
            where_ (t ^. TimeEntryKey ==. val key)

    deleteEntry key =
        delete $ from $ \t ->
            where_ (t ^. TimeEntryKey ==. val key)

    getEntry key = fmap listToMaybe $
        select $ from $ \c -> do
            where_ (c ^. TimeEntryKey ==. val key)
            return c
