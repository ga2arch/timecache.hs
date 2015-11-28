{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache.Server
    ( runHttpServer
    ) where

import           Cache.TimeCache.Model
import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as H
import           Data.Maybe                   (listToMaybe)
import           Data.String.Conversions
import           Data.Text                    (Text, pack)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto           hiding (get)
import           Network.HTTP.Types.Status
import           Web.Scotty.Internal.Types
import qualified Web.Scotty.Trans             as SCT

type TServer = ScottyT Text TimeCache
type TAction = ActionT Text TimeCache

instance ScottyError Text where
    stringError = cs
    showError   = cs

server :: TServer ()
server = do
    SCT.post "/insert" $ do
        entry@(TimeEntry key value time) <- SCT.jsonData-- :: SCT.ActionM TimeEntry

        lift $ do
            cacheEntry entry
            storeEntry entry

        SCT.status status200

    SCT.delete "/delete/:key" $ do
        key <- SCT.param "key"
        liftIO $ putStrLn $ "Deleting: " ++ show key

        mkvStore <- lift getKVStore
        liftIO $ atomically $ modifyTVar' mkvStore $ \kvStore ->
            H.delete key kvStore

        runQuery $ deleteEntry key
        SCT.status status200

    SCT.get "/entries/:key" $ do
        key <- SCT.param "key"
        query <- runQuery $ getEntry key
        case query of
            Just (entityVal -> entry) -> SCT.json entry
            Nothing                   -> SCT.status status404
  where
    runQuery = lift . runDb

runHttpServer :: TimeCache ()
runHttpServer = do
    port   <- getPort
    config <- ask
    state  <- get
    SCT.scottyT port (runT config state) server
