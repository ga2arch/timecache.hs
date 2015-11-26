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

import           Control.Monad.Catch
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Monoid
import           Data.Pool                 (Pool)
import           Data.Text                 (Text, unpack)
import           Database.Esqueleto
import           Database.Persist
import           Database.Persist.TH
import           GHC.Generics              (Generic)

data TimeCacheConfig = TimeCacheConfig {
    webHook :: Text
,   pool    :: Pool SqlBackend
}

newtype TimeCache a = T { unT :: ReaderT TimeCacheConfig IO a }
    deriving (Functor, Applicative, Monad, MonadReader TimeCacheConfig,
              MonadIO, MonadCatch, MonadThrow, MonadMask)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
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
