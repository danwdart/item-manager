{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Types.Item where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data CreateItem = CreateItem {
    createItemName       :: Text,
    createItemCategoryId :: Int
} deriving (Generic, ToJSON, Show)

data Item = Item {
    itemId     :: Int,
    name       :: Text,
    categoryId :: Int
} deriving (Generic, Show, Eq)

instance FromJSON Item where
    parseJSON (Object o) = Item <$> o .: "id" <*> o .: "name" <*> o .: "categoryId"
    parseJSON _ = error "Incorrect use of parsing a item"

instance ToJSON Item where
    toJSON (Item id' name' categoryId') = object ["id" .= id', "name" .= name', "categoryId" .= categoryId']

type ItemsResponse = [Item]
