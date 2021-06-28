

{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Service.Category where

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

import           Types.Category

apiEndpoint ∷ Text
apiEndpoint = "http://localhost:8081/api"

-- Widget, really??

getAllCategories ∷ MonadWidget t m ⇒ Event t () → m (Event t CategoriesResponse)
getAllCategories e = fmap (fromMaybe [Category 0 "Cannot fetch results"]) <$> getAndDecode ((apiEndpoint <> "/categories") <$ e)

getCategory ∷ MonadWidget t m ⇒ Event t Int → m (Event t (Maybe Category))
getCategory e = getAndDecode ((\categoryId' -> apiEndpoint <> "/categories/" <> T.pack (show categoryId')) <$> e)

createCategory ∷ MonadWidget t m ⇒ Event t Text → m (Event t (Maybe Category))
createCategory e = fmap decodeXhrResponse <$> performRequestAsync (
    postJson (apiEndpoint <> "/categories") . CreateCategory <$> e
    )

deleteCategory ∷ MonadWidget t m ⇒ Event t Int → m (Event t (Maybe ()))
deleteCategory e = fmap decodeXhrResponse <$> performRequestAsync (
    (\categoryId' -> XhrRequest "DELETE" (apiEndpoint <> "/categories/" <> T.pack (show categoryId')) def) <$> e
    )

modifyCategory ∷ MonadWidget t m ⇒ Event t Category → m (Event t (Maybe Category))
modifyCategory e = fmap decodeXhrResponse <$> performRequestAsync (
    (\category -> XhrRequest "PUT" (apiEndpoint <> "/categories/" <> T.pack (show (categoryId category))) $ def {
        _xhrRequestConfig_headers = "Content-type" =: "application/json",
        _xhrRequestConfig_sendData = LT.unpack . B.toLazyText . encodeToTextBuilder $ toJSON category
    }) <$> e)
