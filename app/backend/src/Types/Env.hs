{-# LANGUAGE UnicodeSyntax #-}
module Types.Env where

import           Database.SQLite.Simple

newtype Env = Env {
    conn :: Connection
}
