{-# LANGUAGE NoImplicitPrelude #-}
module BinarySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import RIO
import Binary
import Builder
import Data.ByteString.Builder (toLazyByteString)
import Helper ()

spec :: Spec
spec = describe "Binary" $ do
  prop "matches Builder output" $ \scores ->
    binaryEncode scores `shouldBe` toLazyByteString (builder scores)
  prop "idempotent" $ \scores -> -- common QuickCheck property!
    binaryDecode (binaryEncode scores) `shouldBe` Right scores
