{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}


{-# LANGUAGE UnicodeSyntax  #-}

module Types.Category where

import           Data.Aeson
import           Data.Text                        (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Generics
import           Servant

newtype CategoryId = CategoryId Int
    deriving
        (Eq, Show, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, FromField, ToField, Num) via Int

newtype CategoryName = CategoryName Text
    deriving
        (Eq, Show, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, FromField, ToField) via Text

data Category = Category {
    id   :: CategoryId,
    name :: CategoryName
} deriving (Eq, Generic, FromJSON, ToJSON, Show)

newtype CreateCategory = CreateCategory {
    createCategoryName :: CategoryName
} deriving (Generic, FromJSON, ToJSON)

instance FromRow Category where
    fromRow = Category <$> field <*> field

instance ToRow Category where
    toRow (Category id' name') = toRow (id', name')
