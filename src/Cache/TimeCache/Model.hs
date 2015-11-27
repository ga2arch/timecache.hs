{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Cache.TimeCache.Model where

import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Text
import           Database.Esqueleto
import           Database.Persist.TH
import           GHC.Generics        (Generic)
import           Control.Monad.Reader

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

type QueryResult m a = ReaderT SqlBackend m a

insertEntry :: MonadIO m => TimeEntry -> QueryResult m ()
insertEntry entry = void $ insert entry

updateEntry :: MonadIO m => TimeEntry -> QueryResult m ()
updateEntry (TimeEntry key value time) =
    update $ \t -> do
        set t [TimeEntryValue     =. val value,
               TimeEntryTimestamp =. val time]
        where_ (t ^. TimeEntryKey ==. val key)

deleteEntry :: MonadIO m => Text -> QueryResult m ()
deleteEntry key =
    delete $ from $ \t ->
        where_ (t ^. TimeEntryKey ==. val key)

getEntry :: MonadIO m => Text -> QueryResult m (Maybe (Entity TimeEntry))
getEntry key = fmap listToMaybe $
    select $ from $ \c -> do
        where_ (c ^. TimeEntryKey ==. val key)
        return c
