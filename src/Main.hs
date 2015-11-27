{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Cache.TimeCache
import           Data.Text           (pack)
import           Options.Applicative
import           System.Environment

config = TimeCacheConfig
    <$> (fmap pack $ strOption
        (  long "db"
        <> metavar "NAME"
        <> value "timecache.sql"
        <> help "The name of the db file"))

    <*> option auto
        (  long "port"
        <> metavar "PORT"
        <> value 8080
        <> help "The port to listen on")

    <*> (fmap pack $ strOption
        (  long "hook"
        <> metavar "URL"
        <> help "The url of the hook"))

main :: IO ()
main = execParser opts >>= runTimeCache
  where
    opts = info (helper <*> config)
        ( fullDesc
        <> progDesc "Run the cache"
        <> header "timecache - simple cache with expiring events" )
