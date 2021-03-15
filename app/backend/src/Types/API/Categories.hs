{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types.API.Categories where

import           Servant.API
import           Types.Category

type GetAllCategoriesAPI = Get '[JSON] [Category]

type GetCategoryAPI = Capture "id" CategoryId :> Get '[JSON] Category

type DeleteCategoryAPI = Capture "id" CategoryId :> DeleteNoContent '[JSON] NoContent

type PutCategoryAPI = Capture "id" CategoryId :> ReqBody '[JSON] Category :> Put '[JSON] Category

type PostCategoryAPI = ReqBody '[JSON] CreateCategory :> PostCreated '[JSON] Category

type CategoriesAPI = "categories" :> (
        GetAllCategoriesAPI :<|> GetCategoryAPI :<|> DeleteCategoryAPI :<|> PutCategoryAPI :<|> PostCategoryAPI
    )
