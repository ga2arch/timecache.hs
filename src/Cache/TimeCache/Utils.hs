{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Cache.TimeCache.Utils
    ( insertEntry
    , runDb
    , send
    ) where

import           Control.Concurrent.MVar
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as H
import           Data.String.Conversions
import           Data.Text                    (Text, unpack)
import           Database.Persist.Sqlite      (createSqlitePool, runSqlPool)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

insertEntry mh next value = modifyMVar_ mh $ \h -> do
    let bucket = H.lookupDefault [] next h
    return $ H.insert next (value:bucket) h

runDb pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool

send url value = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseUrl $ unpack url

    let request = initialRequest {
        method = "POST"
    ,   requestBody = RequestBodyLBS $ cs value
    }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
        ++ show (statusCode $ responseStatus response)
