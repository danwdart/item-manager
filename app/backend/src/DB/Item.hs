{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DB.Item where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Database.SQLite.Simple
import           Types.Env
import           Types.Item                 as Item

getAll ∷ MonadIO m ⇒ ReaderT Env m [Item]
getAll = do
    conn' <- asks conn
    liftIO $ query_ conn' "SELECT * from items"

get ∷ MonadIO m ⇒ ItemId → ReaderT Env m (Maybe Item)
get itemId = do
    conn' <- asks conn
    items <- liftIO (query conn' "SELECT * from items WHERE id = ? LIMIT 1" (Only itemId) :: IO [Item])
    pure $ case items of
        [item] -> Just item
        _      -> Nothing

delete ∷ MonadIO m ⇒ ItemId → ReaderT Env m ()
delete itemId = do
    conn' <- asks conn
    liftIO $ execute conn' "DELETE FROM items WHERE id = ?" (Only itemId)

update ∷ MonadIO m ⇒ Item → ReaderT Env m ()
update item = do
    conn' <- asks conn
    liftIO $ execute conn' "UPDATE items SET name = ?, categoryId = ? WHERE id = ?" (name item, categoryId item, Item.id item)

create ∷ MonadIO m ⇒ CreateItem → ReaderT Env m Item
create cj = do
    conn' <- asks conn
    liftIO $ execute conn' "INSERT INTO items (name, categoryId) VALUES (?, ?)" (createItemName cj, createItemCategoryId cj)
    rowId <- liftIO $ lastInsertRowId conn'
    pure $ Item {
        Item.name = createItemName cj,
        Item.id = fromIntegral rowId,
        Item.categoryId = createItemCategoryId cj
    }
