{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE UnicodeSyntax     #-}

module Server.Server where

import           Control.Applicative
import           Data.Text           (Text)
import           Servant
import           Servant.API
import           Server.Categories
import           Server.Items
import           Types.API.API
import           Types.App
import           Types.Category
import           Types.Item

apiAPI ∷ App APIAPI
apiAPI = itemsAPI :<|> categoriesAPI

rootAPI ∷ App RootAPI
rootAPI = pure "Hello World!"

appAPI ∷ App API
appAPI = rootAPI :<|> apiAPI
