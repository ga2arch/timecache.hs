{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Server
    ( runHttpServer
    ) where

import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import Cache.TimeCache.Binary
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Maybe                (listToMaybe)
import           Data.String.Conversions
import           Data.Text                 (Text)
import qualified Data.Text.Lazy as TL
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

    now <- liftIO $ round <$> getPOSIXTime
    if (timestamp <= now)
        then void (lift $ post value)
        else lift (insertEntry $ TimeEntry key value timestamp)

  SCT.delete "/delete/:key" $ do
    key <- SCT.param "key"
    lift $ deleteEntry key

  SCT.delete "/prefix/:prefix" $ do
    prefix <- SCT.param "prefix"
    lift $ deleteEntries prefix

  SCT.get "/entries/:key" $ do
    key <- SCT.param "key"
    entry <- lift $ getEntry key
    case entry of
        Just e  -> SCT.raw (BL.fromStrict $ timeEntryValue e)
        Nothing -> SCT.status status404

  SCT.get "/entries/:prefix/:start/:end" $ do
    prefix <- SCT.param "prefix"
    start  <- SCT.param "start"
    end    <- SCT.param "end"
    entries <- lift getEntries
    let count = length $ filter (f prefix start end) entries
    SCT.text $ TL.pack (show count)
    where
      f prefix s e (TimeEntry key _ time) =
        B.isPrefixOf prefix key && time >= s && time <= e

runHttpServer :: TimeCache ()
runHttpServer = do
  port   <- getPort
  config <- ask
  state  <- get
  SCT.scottyT port (runT config state) server
