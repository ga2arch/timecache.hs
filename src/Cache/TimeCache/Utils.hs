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
import qualified Control.Exception            as E
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Pool
import           Data.String.Conversions
import           Data.Text                    (Text, unpack)
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool, runSqlPool)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

runDb :: SqlPersistT (NoLoggingT (ResourceT TimeCache)) a -> TimeCache a
runDb f = do
    pool <- getPool
    runResourceT $ runNoLoggingT $ runSqlPool f pool

post :: Text -> TimeCache (Either HttpException Bool)
post value = do
    hook <- getHook

    manager <- liftIO $ newManager defaultManagerSettings
    initialRequest <- parseUrl $ unpack hook

    let request = initialRequest {
        method = "POST"
    ,   requestBody = RequestBodyLBS $ cs value
    }

    response <- liftIO $ E.try $ httpLbs request manager
    case response of
        Right _                    -> return $ Right True
        Left (ex :: HttpException) -> return $ Left ex

evict :: TimeEntry -> TimeCache ()
evict (TimeEntry key value _) = do
    liftIO $ putStr $ "Evicting: " ++ (unpack value) ++ " ... "

    resp <- post value
    if (isRight resp)
        then do
            runDb $ delete $ from $ \t ->
                        where_ (t ^. TimeEntryKey ==. val key)
            liftIO $ putStrLn " OK."
        else liftIO $ putStrLn " Fail."
