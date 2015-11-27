{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Cache.TimeCache
import           Data.Text          (pack)
import           System.Environment

main :: IO ()
main = do
    host <- getEnv "ACM"
    let hook = pack $ "http://"++host++":8000/expiration"
    let config = TimeCacheConfig "timecache.sql" 8080 hook

    runTimeCache config
