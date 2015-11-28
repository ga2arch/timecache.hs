{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cache.TimeCache.Server
    ( runHttpServer
    ) where

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
import qualified Data.HashTable.IO as H
import           Data.Maybe                   (listToMaybe)
import           Data.String.Conversions
import           Data.Text                    (Text, pack)
import           Data.Time.Clock.POSIX
import           Network.HTTP.Types.Status
import           Web.Scotty.Internal.Types
import qualified Web.Scotty.Trans             as SCT
import GHC.Conc.Sync

type TServer = ScottyT Text TimeCache
type TAction = ActionT Text TimeCache

instance ScottyError Text where
    stringError = cs
    showError   = cs

server :: TServer ()
server = do
    SCT.post "/insert" $ do
        entry@(TimeEntry key value time) <- SCT.jsonData
        lift $ insertEntry entry
        SCT.status status200

    SCT.delete "/delete/:key" $ do
        key <- SCT.param "key"
        lift $ deleteEntry key
        SCT.status status200

    SCT.get "/entries/:key" $ do
        key <- SCT.param "key"
        mkvStore <- lift getKVStore
        entry <- liftIO . atomically $ do
            res <- unsafeIOToSTM $ H.lookup mkvStore key
            case res of
                Just me -> readTVar me >>= return . Just
                Nothing -> return Nothing

        case entry of
            Just e  -> SCT.json e
            Nothing -> SCT.status status404

runHttpServer :: TimeCache ()
runHttpServer = do
    port   <- getPort
    config <- ask
    state  <- get
    SCT.scottyT port (runT config state) server
