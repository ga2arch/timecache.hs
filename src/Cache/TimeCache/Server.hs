{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Server
    ( runHttpServer
    ) where

import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy      as B
import qualified Data.HashTable.IO         as H
import           Data.Maybe                (listToMaybe)
import           Data.String.Conversions
import           Data.Text                 (Text, pack)
import           Data.Time.Clock.POSIX
import           Network.HTTP.Types.Status
import           Web.Scotty.Internal.Types
import qualified Web.Scotty.Trans          as SCT

type TServer = ScottyT Text TimeCache
type TAction = ActionT Text TimeCache

instance ScottyError Text where
    stringError = cs
    showError   = cs

server :: TServer ()
server = do
    SCT.post "/insert" $ do
        key       <- SCT.param "key"
        value     <- SCT.param "value"
        timestamp <- SCT.param "timestamp"
        lift $ insertEntry $ TimeEntry key value timestamp
        SCT.status status200

    SCT.delete "/delete/:key" $ do
        key <- SCT.param "key"
        lift $ deleteEntry key
        SCT.status status200

    SCT.delete "/prefix/:prefix" $ do
        prefix <- SCT.param "prefix"
        lift $ deleteEntries prefix
        SCT.status status200

    SCT.get "/entries/:key" $ do
        key <- SCT.param "key"
        entry <- lift $ getEntry key
        case entry of
            Just e  -> SCT.raw (B.fromStrict $ timeEntryValue e)
            Nothing -> SCT.status status404

runHttpServer :: TimeCache ()
runHttpServer = do
    port   <- getPort
    config <- ask
    state  <- get
    SCT.scottyT port (runT config state) server
