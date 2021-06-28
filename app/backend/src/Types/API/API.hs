{-# LANGUAGE DataKinds     #-}


{-# LANGUAGE DerivingVia   #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types.API.API where

import           Data.Aeson
import           Data.Text            (Text)
import           GHC.Generics
import           Servant
import           Servant.API
import           Types.API.Categories
import           Types.API.Items
import           Types.Category
import           Types.Item

type RootAPI = Get '[PlainText] Text

type APIAPI = "api" :> (ItemsAPI :<|> CategoriesAPI)

type API = RootAPI :<|> APIAPI
