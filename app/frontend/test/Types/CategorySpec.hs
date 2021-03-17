{-# LANGUAGE UnicodeSyntax #-}
module Types.CategorySpec where

import           Data.Aeson
import           Test.Hspec                     (HasCallStack, Spec, describe,
                                                 it, parallel, runIO, shouldBe,
                                                 shouldSatisfy, xdescribe)
import           Test.Hspec.Expectations        (shouldNotBe, shouldNotContain)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text
import           Types.Category

instance Arbitrary Category where
    arbitrary = Category <$> arbitrary <*> arbitrary

propSerialiseUnserialise ∷ Category → Property
propSerialiseUnserialise category =
    decode (encode category) === Just category

spec ∷ Spec
spec = describe "Category" .
    it "serialises to JSON and unserialises" $
        quickCheck propSerialiseUnserialise
