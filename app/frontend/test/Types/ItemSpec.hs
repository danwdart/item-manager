{-# LANGUAGE UnicodeSyntax #-}
module Types.ItemSpec where

import           Data.Aeson
import           Test.Hspec                     (HasCallStack, Spec, describe,
                                                 it, parallel, runIO, shouldBe,
                                                 shouldSatisfy, xdescribe)
import           Test.Hspec.Expectations        (shouldNotBe, shouldNotContain)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text
import           Types.Category                 as Category
import           Types.CategorySpec             hiding
                                                (propSerialiseUnserialise)
import           Types.Item

instance Arbitrary Item where
    arbitrary = Item <$> arbitrary <*> arbitrary <*> arbitrary

propSerialiseUnserialise ∷ Item → Property
propSerialiseUnserialise item =
    decode (encode item) === Just item

spec ∷ Spec
spec = describe "Item" .
    it "serialises to JSON and unserialises" $
        quickCheck propSerialiseUnserialise
