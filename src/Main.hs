{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.ByteString         (ByteString)
import           Data.Hashable           (Hashable)
import           Data.Monoid
import           Data.Streaming.Network
import           Data.Time.Clock.POSIX

import qualified Data.ByteString.Char8   as C
import qualified Data.HashMap.Strict     as H

data Result = Done
            | Entry !ByteString !Int
            | Error !ByteString
    deriving (Show)

forkOnNew ad value time = forkIO $ do
    threadDelay $ time * 10^6
    appWrite ad $ value <> "\n"

mapBased mh ad value time = do
    current <- round <$> getPOSIXTime
    let next = current + time
    modifyMVar_ mh $ \h -> do
        let bucket = H.lookupDefault [] next h
        return $ H.insert next (value:bucket) h

worker :: MVar (H.HashMap Int [ByteString]) -> IO ()
worker mh = do
    current <- round <$> getPOSIXTime
    mapM_ handle $ iterate (+1) current

  where
    handle current = do
        modifyMVar_ mh $ \h ->
            maybe (return h)
                  (f h current)
                  (H.lookup current h)

        threadDelay $ 1 * 10^6

    f h current bucket = do
        async $ mapM_ (\x -> print $ show x <> "timed out") bucket
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

main :: IO ()
main = do
    h <- newMVar H.empty
    async $ worker h
    tcpServer $ mapBased h
