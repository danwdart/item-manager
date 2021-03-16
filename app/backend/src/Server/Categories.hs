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
import           DB.Category                as DBCategory
import           Servant
import           Servant.API
import           Types.API.API
import           Types.API.Categories
import           Types.App
import           Types.Category             as Category
import           Types.Env

getAllCategoriesAPI ∷ App GetAllCategoriesAPI
getAllCategoriesAPI = DBCategory.getAll

getCategoryAPI ∷ App GetCategoryAPI
getCategoryAPI categoryId = do
    categories <- DBCategory.get categoryId
    case categories of
        Just category -> pure category
        _ -> throwError $ err404 {
            errBody = "Specified category not found"
        }

deleteCategoryAPI ∷ App DeleteCategoryAPI
deleteCategoryAPI categoryId = NoContent <$ DBCategory.delete categoryId

putCategoryAPI ∷ App PutCategoryAPI
putCategoryAPI categoryId newCategory = do
    conn' <- asks conn
    mCategory <- DBCategory.get categoryId
    case mCategory of
        Just category -> do
            DBCategory.update newCategory
            pure newCategory
        _ -> throwError $ err404 {
            errBody = "Specified category not found"
        }

postCategoryAPI ∷ App PostCategoryAPI
postCategoryAPI = DBCategory.create

categoriesAPI ∷ App CategoriesAPI
categoriesAPI = getAllCategoriesAPI :<|> getCategoryAPI :<|> deleteCategoryAPI :<|> putCategoryAPI :<|> postCategoryAPI
