{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}


{-# LANGUAGE UnicodeSyntax  #-}

module Types.Item where

import           Data.Aeson
import           Data.Text                        (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Generics
import           Servant
import           Types.Category                   (CategoryId)
newtype ItemId = ItemId Int
    deriving
        (Show, Eq, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, FromField, ToField, Num) via Int

newtype ItemName = ItemName Text
    deriving
        (Show, Eq, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, FromField, ToField) via Text

data Item = Item {
    id         :: ItemId,
    name       :: ItemName,
    categoryId :: CategoryId
} deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CreateItem = CreateItem {
    createItemName       :: ItemName,
    createItemCategoryId :: CategoryId
} deriving (Generic, FromJSON, ToJSON)

instance FromRow Item where
    fromRow = Item <$> field <*> field <*> field

instance ToRow Item where
    toRow (Item id' name' categoryId') = toRow (id', name', categoryId')
