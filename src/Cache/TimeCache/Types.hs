{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Cache.TimeCache.Types where

import           Data.Aeson
import           Data.Monoid
import           Data.Text           (Text, unpack)
import           Database.Persist.TH
import           GHC.Generics        (Generic)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Webhook
    endpoint Text
    deriving Show

TimeEntry
   key       Text
   value     Text
   timestamp Int

   deriving Show
   deriving Generic
|]

instance ToJSON   TimeEntry
instance FromJSON TimeEntry where
    parseJSON (Object v) = TimeEntry <$>
                          v .: "key"   <*>
                          v .: "value" <*>
                          v .: "timestamp"
    parseJSON _          = mempty
