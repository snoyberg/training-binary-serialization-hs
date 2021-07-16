{-# LANGUAGE NoImplicitPrelude #-}
module PersistSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import RIO
import qualified RIO.ByteString.Lazy as BL
import Persist
import Builder
import Data.ByteString.Builder (toLazyByteString)
import Helper ()
import qualified RIO.Vector as V

spec :: Spec
spec = describe "Persist" $ do
  prop "matches Builder output" $ \scores ->
    BL.fromChunks [persistEncode scores] `shouldBe` toLazyByteString (builder scores)
  prop "idempotent" $ \scores ->
    persistDecode (persistEncode scores) `shouldBe` Right scores
