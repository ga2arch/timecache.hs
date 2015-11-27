{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.TimeCache.Utils
    ( runDb
    , evict
    ) where

import           Cache.TimeCache.Model
import           Cache.TimeCache.Types
import           Control.Concurrent.MVar
import qualified Control.Exception            as E
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Either
import qualified Data.HashMap.Strict          as H
import           Data.Pool
import           Data.String.Conversions
import           Data.Text                    (Text, unpack)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool, runSqlPool)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

runDb pool f = runResourceT $ runNoLoggingT $ runSqlPool f pool

post :: Text -> Text -> IO (Either HttpException Bool)
post url value = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseUrl $ unpack url

    let request = initialRequest {
        method = "POST"
    ,   requestBody = RequestBodyLBS $ cs value
    }

    response <- E.try $ httpLbs request manager
    case response of
        Right _ -> return $ Right True
        Left (ex :: HttpException) -> return $ Left ex

evict :: Text -> Pool SqlBackend -> TimeEntry -> IO ()
evict hook pool (TimeEntry key value _) = do
    putStr $ "Evicting: " ++ (unpack value) ++ " ... "

    resp <- post hook value
    if (isRight resp)
        then do
            runDb pool $ delete $ from $ \t ->
                    where_ (t ^. TimeEntryKey ==. val key)
            putStrLn " OK"
        else putStrLn " OK"
