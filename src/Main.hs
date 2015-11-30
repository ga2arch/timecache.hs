{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Cache.TimeCache
import           Data.Text           (pack)
import           Options.Applicative
import           System.Environment

config = TimeCacheConfig
    <$> option auto
        (  long "port"
        <> metavar "PORT"
        <> value 8080
        <> help "The port to listen on")

    <*> txtOption
        (  long "hook"
        <> metavar "URL"
        <> help "The url of the hook")

    <*> option auto
        ( long "interval"
        <> metavar "TIME"
        <> value 1
        <> help "The interval between two checks")
  where
    txtOption = fmap pack . strOption

main :: IO ()
main = execParser opts >>= runTimeCache
  where
    opts = info (helper <*> config)
        ( fullDesc
        <> progDesc "Run the cache"
        <> header "timecache - simple cache with expiring events" )
        
