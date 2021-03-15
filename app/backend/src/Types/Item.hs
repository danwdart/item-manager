{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Types.Item where

import           Data.Aeson
import           Data.Text                        (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Generics
import           Servant

newtype ItemId = ItemId Int
    deriving
        (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, FromField, ToField, Num) via Int

newtype ItemName = ItemName Text
    deriving
        (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, FromField, ToField) via Text

data Item = Item {
    id   :: ItemId,
    name :: ItemName
} deriving (Generic, FromJSON, ToJSON)

newtype CreateItem = CreateItem {
    createItemName :: ItemName
} deriving (Generic, FromJSON, ToJSON)

instance FromRow Item where
    fromRow = Item <$> field <*> field

instance ToRow Item where
    toRow (Item id' name') = toRow (id', name')
