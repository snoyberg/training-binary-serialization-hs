{-# LANGUAGE NoImplicitPrelude #-}
module BuilderSpec (spec) where

import           Builder
import           Data.ByteString.Builder (toLazyByteString)
import           Helper                  ()
import           RIO
import qualified RIO.ByteString.Lazy     as BL
import qualified RIO.Vector              as V
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Builder" $ do
    -- Use length to force the value
    prop "returns a non-bottom value" $ \scores -> do
      BL.length (toLazyByteString $ builder scores) `shouldSatisfy` (> 0)
    prop "additional input makes the output longer" $ \score1 score2 -> do
      let len1 = BL.length $ toLazyByteString $ builder $ V.singleton score1
          len2 = BL.length $ toLazyByteString $ builder $ V.singleton score2
          lenBoth = BL.length $ toLazyByteString $ builder $ V.fromList [score1, score2]
      lenBoth `shouldSatisfy` (> len1)
      lenBoth `shouldSatisfy` (> len2)
      len1 + len2 `shouldSatisfy` (> lenBoth)
