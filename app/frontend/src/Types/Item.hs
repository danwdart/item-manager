{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Item where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype CreateItem = CreateItem {
    createItemName :: Text
} deriving (Generic, ToJSON, Show)

data Item = Item {
    itemId :: Int,
    name  :: Text
} deriving (Generic, Show)

instance FromJSON Item where
    parseJSON (Object o) = Item <$> o .: "id" <*> o .: "name"
    parseJSON _ = error "Incorrect use of parsing a item"

instance ToJSON Item where
    toJSON (Item id' name') = object ["id" .= id', "name" .= name']

type ItemsResponse = [Item]