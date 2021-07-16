module Data.Text.Show where

import Data.Text (Text, pack)

tshow :: (Show a) => a -> Text
tshow = pack . show