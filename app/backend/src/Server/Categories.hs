{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Server.Categories where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Text                  (Text)
import           Database.SQLite.Simple
import           Servant
import           Servant.API
import           Types.API.API
import           Types.API.Categories
import           Types.App
import           Types.Category              as Category
import           Types.Env

getAllCategoriesAPI ∷ App GetAllCategoriesAPI
getAllCategoriesAPI = do
    conn' <- asks conn
    liftIO (query_ conn' "SELECT * from categories" :: IO [Category])

getCategoryAPI ∷ App GetCategoryAPI
getCategoryAPI categoryId = do
    conn' <- asks conn
    categories <- liftIO (query conn' "SELECT * from categories WHERE id = ? LIMIT 1" (Only categoryId) :: IO [Category])
    case categories of
        [category] -> pure category
        _ -> throwError $ err404 {
            errBody = "Specified category not found"
        }

deleteCategoryAPI ∷ App DeleteCategoryAPI
deleteCategoryAPI categoryId = do
    conn' <- asks conn
    liftIO $ execute conn' "DELETE FROM categories WHERE id = ?" (Only categoryId)
    pure NoContent

putCategoryAPI ∷ App PutCategoryAPI
putCategoryAPI categoryId newCategory = do
    conn' <- asks conn
    categories <- liftIO (query conn' "SELECT * from categories WHERE id = ? LIMIT 1" (Only categoryId) :: IO [Category])
    case categories of
        [category] -> do
            liftIO $ execute conn' "UPDATE categories SET name = ? WHERE id = ?" (name newCategory, categoryId)
            pure newCategory
        _ -> throwError $ err404 {
            errBody = "Specified category not found"
        }

postCategoryAPI ∷ App PostCategoryAPI
postCategoryAPI cj = do
    conn' <- asks conn
    liftIO $ execute conn' "INSERT INTO categories (name) VALUES (?)" (Only (createCategoryName cj))
    rowId <- liftIO $ lastInsertRowId conn'
    pure $ Category { Category.name = createCategoryName cj, Category.id = fromIntegral rowId }

categoriesAPI ∷ App CategoriesAPI
categoriesAPI = getAllCategoriesAPI :<|> getCategoryAPI :<|> deleteCategoryAPI :<|> putCategoryAPI :<|> postCategoryAPI
