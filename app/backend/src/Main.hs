{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

-- import           Data.Aeson
-- import           Data.CSV.Import
import           Control.Applicative
import           Control.Monad.Trans.Reader
import           Data.Proxy
import           Data.String
import           Data.Text                   (Text)
import           Database.SQLite.Simple
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.API
import           Server.Server
import           System.IO
import           Types.API.API
import           Types.App
import           Types.Env

api ∷ Proxy API
api = Proxy

rootApp ∷ Env → Application
rootApp env = serve api $ hoistServer api (`runReaderT` env) appAPI

corsResourcePolicy ∷ CorsResourcePolicy
corsResourcePolicy = simpleCorsResourcePolicy {
    corsOrigins = Just (["http://localhost:8080"], True),
    corsMethods = ["GET", "POST", "DELETE", "PUT"],
    corsRequestHeaders = ["content-type"],
    corsRequireOrigin = False,
    corsIgnoreFailures = True
}

main ∷ IO ()
main = do
    putStrLn "Opening database connection..."
    conn <- open "data/items.db"
    putStrLn "Database connection open. Migrating DB..."
    dbFile <- readFile "db/db.sql"
    mapM_ (execute_ conn) (fromString <$> lines dbFile)
    putStrLn "Migrations ran successfully. Running app on port 8081, press ^C to quit..."
    run 8081 . cors (const $ Just corsResourcePolicy) $ rootApp Env { conn = conn }
    putStrLn "App has been quit."
