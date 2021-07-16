{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper
  (
  ) where

import RIO
import qualified RIO.Text as T
import Types
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances ()

instance Arbitrary Score where
  arbitrary = Score
    <$> (T.pack <$> arbitrary)
    <*> (T.pack <$> arbitrary)
    <*> arbitrary
