{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module UI.Bootstrap.TabbedNav where

import Data.Text
import Reflex.Dom

bsTabbedNav :: (MonadWidget t m, Eq a, Semigroup a) => Text -> a -> [(a, Text, m ())] -> m ()
bsTabbedNav theTitle defaultVal items = mdo
    dNav <- holdDyn defaultVal eNavClick
    eNavClick <- divClass "navbar-collapse" .
        elClass "nav" "navbar-expand-lg navbar-light bg-light" .
            elClass "ul" "nav navbar-nav" $ do
                elAttr "a" [("class", "navbar-brand"), ("href", "javascript:void()")] $ text theTitle
                buttons <- mapM (\(val, btnText, _) -> do
                    (btn, _) <- elDynAttr' "a" ((\n -> [("class", "nav-link" <> if n == val then " active" else ""), ("href", "javascript:void()")]) <$> dNav) $ text btnText
                    pure $ val <$ domEvent Click btn
                    ) items
                pure $ mconcat buttons
    divClass "row" . divClass "col" .
        divClass "tab-content" $ mapM_ (\(val, _, content) ->
            elDynAttr "div" ((\n -> [("class", "tab-pane fade" <> if n == val then " show active" else "")]) <$> dNav) content
            ) items