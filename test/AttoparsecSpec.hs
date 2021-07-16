{-# LANGUAGE NoImplicitPrelude #-}
module AttoparsecSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import RIO
import qualified RIO.ByteString.Lazy as BL
import Attoparsec
import Builder
import Data.ByteString.Builder (toLazyByteString)
import Helper ()
import qualified RIO.Vector as V

spec :: Spec
spec = describe "Attoparsec" $ do
  prop "can parse Builder output" $ \scores ->
    either impureThrow id (attoparsec (toLazyByteString (builder scores))) `shouldBe` scores
