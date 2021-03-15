{-# LANGUAGE UnicodeSyntax #-}
module Types.App where

import           Control.Monad.Trans.Reader
import           Servant
import           Servant.API
import           Types.Env

type AppM = ReaderT Env Handler

type App a = ServerT a AppM
