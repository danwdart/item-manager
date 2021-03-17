{-# LANGUAGE UnicodeSyntax #-}
module MainSpec where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.String
import           Database.SQLite.Simple
import           Servant
import           Servant.QuickCheck
import           Server.Server
import           System.Directory
import           Test.Hspec                 (HasCallStack, Spec, describe, it,
                                             parallel, runIO, shouldBe,
                                             shouldSatisfy, xdescribe)
import           Test.Hspec.Expectations    (shouldNotBe, shouldNotContain)
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Types.API.API
import           Types.App
import           Types.Category
import           Types.CategorySpec
import           Types.Env
import           Types.Item
import           Types.ItemSpec

instance Arbitrary CreateCategory where
    arbitrary = CreateCategory <$> arbitrary

instance Arbitrary CreateItem where
    arbitrary = CreateItem <$> arbitrary <*> arbitrary

api ∷ Proxy API
api = Proxy

rootApp ∷ Connection → Server API
rootApp conn = hoistServer api (`runReaderT` Env {
    conn = conn
}) appAPI

spec ∷ Spec
spec = describe "API" .
  it "follows best practices" $ do
    conn <- open "/tmp/test.db"
    liftIO $ do
        dbFile <- readFile "db/db.sql"
        mapM_ (execute_ conn) (fromString <$> lines dbFile)
    withServantServer api (pure (rootApp conn)) $ \burl ->
      serverSatisfies api burl stdArgs (
          not500 <%>
          honoursAcceptHeader <%>
          mempty)
    removeFile "/tmp/test.db"
