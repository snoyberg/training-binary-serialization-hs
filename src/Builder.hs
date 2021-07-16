{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Builder (builder) where

import           Data.ByteString.Builder (byteString, word64BE, word8)
import           RIO
import qualified RIO.ByteString          as B
import qualified RIO.Vector              as V
import           Types

builder :: Vector Score -> Builder
builder v = word64BE (fromIntegral (V.length v)) <> foldMap single v

single :: Score -> Builder
single Score {..} = text firstName <> text lastName <> encodeMaybe word8 value

-- | We encode a 'Text' with the byte count first for efficient decoding,
-- instead of some other scheme like trailing null byte. We need to encode to a
-- 'ByteString' first though in order to get the length, which is a performance
-- hit unfortunately.
text :: Text -> Builder
text t =
  let bs = encodeUtf8 t
   in word64BE (fromIntegral (B.length bs)) <> byteString bs

-- | Encode a maybe value by putting an extra byte in front. If it's 0, it's a
-- maybe. If it's 1, it's Just and is followed by the value.
encodeMaybe :: (a -> Builder) -> Maybe a -> Builder
encodeMaybe _ Nothing  = word8 0
encodeMaybe f (Just x) = word8 1 <> f x
