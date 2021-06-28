

{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Service.Item where

import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as B
import           GHC.Generics

import           Reflex.Dom
import           Reflex.Dom.Xhr

import           Types.Item

apiEndpoint ∷ Text
apiEndpoint = "http://localhost:8081/api"

-- Widget, really??

getAllItems ∷ MonadWidget t m ⇒ Event t () → m (Event t ItemsResponse)
getAllItems e = fmap (fromMaybe [Item 0 "Cannot fetch results" 0]) <$> getAndDecode (apiEndpoint <> "/items" <$ e)

getItem ∷ MonadWidget t m ⇒ Event t Int → m (Event t (Maybe Item))
getItem e = getAndDecode ((\itemId' -> apiEndpoint <> "/items/" <> T.pack (show itemId')) <$> e)

createItem ∷ MonadWidget t m ⇒ Event t CreateItem → m (Event t (Maybe Item))
createItem createItemData = fmap decodeXhrResponse <$> performRequestAsync (
    postJson (apiEndpoint <> "/items") <$> createItemData
    )

deleteItem ∷ MonadWidget t m ⇒ Event t Int → m (Event t (Maybe ()))
deleteItem e = fmap decodeXhrResponse <$> performRequestAsync (
    (\itemId' -> XhrRequest "DELETE" (apiEndpoint <> "/items/" <> T.pack (show itemId')) def) <$> e
    )

modifyItem ∷ MonadWidget t m ⇒ Event t Item → m (Event t (Maybe Item))
modifyItem e = fmap decodeXhrResponse <$> performRequestAsync (
    (\item -> XhrRequest "PUT" (apiEndpoint <> "/items/" <> T.pack (show (itemId item))) $ def {
        _xhrRequestConfig_headers = "Content-type" =: "application/json",
        _xhrRequestConfig_sendData = LT.unpack . B.toLazyText . encodeToTextBuilder $ toJSON item
    }) <$> e)
