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

import           GHC.Generics                 (Generic)
import           Data.Aeson                   (FromJSON, ToJSON, encode)
import           Database.Persist.TH
import           Data.Text                    (Text, unpack)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Webhook
    endpoint Text
    deriving Show

TimeEntry
   value     Text
   timestamp Int

   deriving Show
   deriving Generic
|]

instance ToJSON   TimeEntry
instance FromJSON TimeEntry
