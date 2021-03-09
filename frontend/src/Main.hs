{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

main :: IO ()
main = do
    mainWidgetWithHead (do
        elAttr "link" [
            ("href", "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/css/bootstrap.min.css"),
            ("rel", "stylesheet"),
            ("integrity", "sha384-BmbxuPwQa2lc/FVzBcNJ7UAyJxM6wuqIj61tLrc4wSX0szH/Ev+nYRRuWlolflfl"),
            ("crossorigin", "anonymous")
            ] $ blank
        elAttr "meta" [
            ("charset", "utf-8")
            ] $ blank
        ) $ do
            el "h1" $ text "Item Manager"
            pure ()