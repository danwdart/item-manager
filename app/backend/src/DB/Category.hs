{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DB.Category where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Database.SQLite.Simple
import           Types.Category             as Category
import           Types.Env

getAll ∷ MonadIO m ⇒ ReaderT Env m [Category]
getAll = do
    conn' <- asks conn
    liftIO $ query_ conn' "SELECT id, name from categories"

get ∷ MonadIO m ⇒ CategoryId → ReaderT Env m (Maybe Category)
get categoryId = do
    conn' <- asks conn
    categories <- liftIO (query conn' "SELECT id, name from categories WHERE id = ? LIMIT 1" (Only categoryId) :: IO [Category])
    pure $ case categories of
        [category] -> Just category
        _          -> Nothing

delete ∷ MonadIO m ⇒ CategoryId → ReaderT Env m ()
delete categoryId = do
    conn' <- asks conn
    liftIO $ execute conn' "DELETE FROM categories WHERE id = ?" (Only categoryId)

update ∷ MonadIO m ⇒ Category → ReaderT Env m ()
update category = do
    conn' <- asks conn
    liftIO $ execute conn' "UPDATE categories SET name = ? WHERE id = ?" (name category, Category.id category)

create ∷ MonadIO m ⇒ CreateCategory → ReaderT Env m Category
create cj = do
    conn' <- asks conn
    liftIO $ execute conn' "INSERT INTO categories (name) VALUES (?)" (Only (createCategoryName cj))
    rowId <- liftIO $ lastInsertRowId conn'
    pure $ Category {
        Category.name = createCategoryName cj,
        Category.id = fromIntegral rowId
    }
