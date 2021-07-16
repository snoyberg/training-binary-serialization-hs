{-# LANGUAGE NoImplicitPrelude #-}
module Types
  ( Score (..)
  ) where

import RIO

data Score = Score
  { firstName :: !Text
  , lastName :: !Text
  , value :: !(Maybe Word8)
  }
  deriving (Show, Eq)
