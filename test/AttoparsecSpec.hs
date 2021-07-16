{-# LANGUAGE NoImplicitPrelude #-}
module AttoparsecSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import RIO
import Attoparsec
import Builder
import Data.ByteString.Builder (toLazyByteString)
import Helper ()

spec :: Spec
spec = describe "Attoparsec" $ do
  prop "can parse Builder output" $ \scores ->
    either impureThrow id (attoparsec (toLazyByteString (builder scores))) `shouldBe` scores
