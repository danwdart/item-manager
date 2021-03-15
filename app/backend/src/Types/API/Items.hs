{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types.API.Items where

import qualified Data.Item    as ItemCSV
import           Servant.API
import           Types.Item

type GetAllItemsAPI = Get '[JSON] [Item]

type GetItemAPI = Capture "id" ItemId :> Get '[JSON] Item

type DeleteItemAPI = Capture "id" ItemId :> DeleteNoContent '[JSON] NoContent

type PutItemAPI = Capture "id" ItemId :> ReqBody '[JSON] Item :> Put '[JSON] Item

type PostItemAPI = ReqBody '[JSON] CreateItem :> PostCreated '[JSON] Item

type ItemsAPI = "items" :> (
        GetAllItemsAPI :<|> GetItemAPI :<|> DeleteItemAPI :<|> PutItemAPI :<|> PostItemAPI
    )
