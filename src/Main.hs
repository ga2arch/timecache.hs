{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Cache.TimeCache
import           Cache.TimeCache.Server
import           Cache.TimeCache.Types
import           Cache.TimeCache.Utils
import           Cache.TimeCache.Worker
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict          as H
import           Database.Esqueleto
import           Database.Persist.Sqlite      (createSqlitePool)

main :: IO ()
main = do
    pool  <- runNoLoggingT $ createSqlitePool "timecache.sql" 5
    runDb pool $ runMigration migrateTables

    let config = TimeCacheConfig "http://104.197.125.254:8000/expiration" pool
    runTimeCache config
