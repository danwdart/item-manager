{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module UI.Bootstrap.Button where

import           Data.Text
import           Reflex.Dom

bsButton ∷ MonadWidget t m ⇒ Text → Text -> m (Event t ())
bsButton class' label = do
    (buttonEl, _) <- elAttr' "button" [("class", class')] $ text label
    pure $ domEvent Click buttonEl
