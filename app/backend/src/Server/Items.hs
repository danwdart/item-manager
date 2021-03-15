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
import           Servant
import           Servant.API
import           Types.API.API
import           Types.API.Items
import           Types.App
import           Types.Env
import           Types.Item                  as Item

getAllItemsAPI ∷ App GetAllItemsAPI
getAllItemsAPI = do
    conn' <- asks conn
    liftIO (query_ conn' "SELECT * from items" :: IO [Item])

getItemAPI ∷ App GetItemAPI
getItemAPI itemId = do
    conn' <- asks conn
    items <- liftIO (query conn' "SELECT * from items WHERE id = ? LIMIT 1" (Only itemId) :: IO [Item])
    case items of
        [item] -> pure item
        _ -> throwError $ err404 {
            errBody = "Specified item not found"
        }

deleteItemAPI ∷ App DeleteItemAPI
deleteItemAPI itemId = do
    conn' <- asks conn
    liftIO $ execute conn' "DELETE FROM items WHERE id = ?" (Only itemId)
    pure NoContent

putItemAPI ∷ App PutItemAPI
putItemAPI itemId item = do
    conn' <- asks conn
    items <- liftIO (query conn' "SELECT * from items WHERE id = ? LIMIT 1" (Only itemId) :: IO [Item])
    case items of
        [item] -> do
            liftIO $ execute conn' "UPDATE items SET name = ? WHERE id = ?" (name item, itemId)
            pure item
        _ -> throwError $ err404 {
            errBody = "Specified item not found"
        }

postItemAPI ∷ App PostItemAPI
postItemAPI cj = do
    conn' <- asks conn
    liftIO $ execute conn' "INSERT INTO items (name) VALUES (?)" (Only (createItemName cj))
    rowId <- liftIO $ lastInsertRowId conn'
    pure $ Item { Item.name = createItemName cj, Item.id = fromIntegral rowId }

itemsAPI ∷ App ItemsAPI
itemsAPI = getAllItemsAPI :<|> getItemAPI :<|> deleteItemAPI :<|> putItemAPI :<|> postItemAPI
