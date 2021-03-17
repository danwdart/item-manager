{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Category where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype CreateCategory = CreateCategory {
    createCategoryName :: Text
} deriving (Generic, ToJSON, Show)

data Category = Category {
    categoryId :: Int,
    name  :: Text
} deriving (Generic, Show, Eq)

instance FromJSON Category where
    parseJSON (Object o) = Category <$> o .: "id" <*> o .: "name"
    parseJSON _ = error "Incorrect use of parsing a category"

instance ToJSON Category where
    toJSON (Category id' name') = object ["id" .= id', "name" .= name']

type CategoriesResponse = [Category]