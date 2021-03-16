{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Server.Items where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Text                  (Text)
import           Database.SQLite.Simple
import           DB.Item                    as DBItem
import           Servant
import           Servant.API
import           Types.API.API
import           Types.API.Items
import           Types.App
import           Types.Env
import           Types.Item                 as Item

getAllItemsAPI ∷ App GetAllItemsAPI
getAllItemsAPI = DBItem.getAll

getItemAPI ∷ App GetItemAPI
getItemAPI itemId = do
    items <- DBItem.get itemId
    case items of
        Just item -> pure item
        _ -> throwError $ err404 {
            errBody = "Specified item not found"
        }

deleteItemAPI ∷ App DeleteItemAPI
deleteItemAPI itemId = NoContent <$ DBItem.delete itemId

putItemAPI ∷ App PutItemAPI
putItemAPI itemId newItem = do
    mItem <- DBItem.get itemId
    case mItem of
        Just item -> do
            DBItem.update newItem
            pure newItem
        Nothing -> throwError $ err404 {
            errBody = "Specified item not found"
        }

postItemAPI ∷ App PostItemAPI
postItemAPI = DBItem.create

itemsAPI ∷ App ItemsAPI
itemsAPI = getAllItemsAPI :<|> getItemAPI :<|> deleteItemAPI :<|> putItemAPI :<|> postItemAPI
