{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Hashable                (Hashable)
import           Data.Monoid
import           Data.Pool                    (Pool)
import           Data.Streaming.Network
import           Data.Text                    (Text)
import           Data.Time.Clock.POSIX
import           Database.Esqueleto
import           Database.Persist             (selectList)
import           Database.Persist.Sqlite      (createSqlitePool, runSqlPool)
import           Database.Persist.TH

import qualified Data.ByteString.Char8        as C
import qualified Data.HashMap.Strict          as H

data Result = Done
            | Entry !ByteString !Int
            | Error !ByteString
    deriving (Show)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
TimeEntry
   value     ByteString
   timestamp Int

   deriving Show
|]

insertEntry mh next value = modifyMVar_ mh $ \h -> do
    let bucket = H.lookupDefault [] next h
    return $ H.insert next (value:bucket) h

strategy :: MVar (H.HashMap Int [ByteString])
        -> Pool SqlBackend
        -> t
        -> ByteString
        -> Int
        -> IO ()
strategy mh pool ad value time = do
    current <- round <$> getPOSIXTime
    let next = current + time
    runDb pool $ insert $ TimeEntry value next

    insertEntry mh next value

worker :: MVar (H.HashMap Int [ByteString])
        -> Pool SqlBackend
        -> IO ()
worker mh pool = do
    now <- round <$> getPOSIXTime
    mapM_ handle $ iterate (+1) now

  where
    handle current = do
        modifyMVar_ mh $ \h ->
            maybe (return h)
                  (f h current)
                  (H.lookup current h)

        threadDelay $ 1 * 10^6

    f h current bucket = do
        async $ do
            mapM_ (\x ->
                print $ show x <> "timed out") bucket

            runDb pool $ delete $ from $ \t ->
                where_ (t ^. TimeEntryTimestamp ==. val current)

        return $ H.delete current h

tcpServer action = runTCPServer (serverSettingsTCP 8080 "*") app
  where
    app ad = do
        bs <- appRead ad
        print $ "Received: " ++ show bs

        case handle bs ad of
            Entry value time -> do
                appWrite ad "Ok\n"
                action ad value time
                app ad

            Done    -> appCloseConnection ad
            Error m -> do
                appWrite ad $ m <> "\n"
                app ad

    handle d ad =
        if d == "done\r\n"
            then Done
            else case C.words d of
                    (x:xs:_) -> mkEntry x xs
                    _        -> Error "Invalid"

    mkEntry value rest =
        case C.readInt rest of
            Just (t, _) -> Entry value t
            Nothing     -> Error "Invalid"

runDb conn f = runResourceT $ runNoLoggingT $ runSqlPool f conn

main :: IO ()
main = do
    h <- newMVar H.empty

    pool <- runNoLoggingT $ createSqlitePool "timecache.sql" 5
    runDb pool $ runMigration migrateTables
    entries <- runDb pool $ select $ from $ \t -> return t

    mapM_ (\x -> do
        now <- round <$> getPOSIXTime

        let time  = timeEntryTimestamp $ entityVal x
            value = timeEntryValue     $ entityVal x

        when (time >= now) $ insertEntry h time value) entries

    async $ worker h pool
    tcpServer $ strategy h pool
