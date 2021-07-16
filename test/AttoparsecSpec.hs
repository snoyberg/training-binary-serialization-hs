{-# LANGUAGE NoImplicitPrelude #-}
module AttoparsecSpec (spec) where

import           Attoparsec
import           Builder
import           Data.ByteString.Builder (toLazyByteString)
import           Helper                  ()
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Attoparsec" $ do
  prop "can parse Builder output" $ \scores ->
    either impureThrow id (attoparsec (toLazyByteString (builder scores))) `shouldBe` scores
