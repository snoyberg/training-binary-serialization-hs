{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Persist
  ( persistEncode
  , persistDecode
  ) where

import RIO
import Types
import Data.Persist
import qualified RIO.ByteString as B
import qualified RIO.Vector as V
import Data.Vector.Binary ()

persistEncode :: Vector Score -> ByteString
persistEncode = encode . Scores

persistDecode :: ByteString -> Either String (Vector Score)
persistDecode = fmap getScores . decode

-- | Avoid the Vector orphan instance
newtype Scores = Scores { getScores :: Vector Score }

instance Persist Scores where
  put (Scores v) = do
    putBE (fromIntegral (V.length v) :: Word64)
    V.mapM_ put v
  get = Scores <$> do
    len :: Word64 <- getBE
    V.replicateM (fromIntegral len) get

instance Persist Score where
  put Score {..} = putText firstName *> putText lastName *> putMaybeWord8 value
  get = Score <$> getText <*> getText <*> getMaybeWord8

putText :: Text -> Put ()
putText t = do
  let bs = encodeUtf8 t
  putBE (fromIntegral $ B.length bs :: Word64)
  putByteString bs

putMaybeWord8 :: Maybe Word8 -> Put ()
putMaybeWord8 Nothing = put (0 :: Word8) -- no endianness choice for Word8
putMaybeWord8 (Just x) = put (1 :: Word8) *> put x

getText :: Get Text
getText = do
  len :: Word64 <- getBE
  bs <- getBytes $ fromIntegral len
  case decodeUtf8' bs of
    Left e -> fail $ "Invalid Utf8: " ++ show e
    Right t -> pure t

getMaybeWord8 :: Get (Maybe Word8)
getMaybeWord8 = do
  marker <- get -- no endianness choice for Word8
  case marker :: Word8 of
    0 -> pure Nothing
    1 -> Just <$> get
    _ -> fail $ "Invalid marker: " ++ show marker
