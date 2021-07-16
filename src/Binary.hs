{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Binary
  ( binaryEncode
  , binaryDecode
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Vector.Binary  ()
import           RIO
import qualified RIO.ByteString      as B
import qualified RIO.ByteString.Lazy as BL
import           Types

binaryEncode :: Vector Score -> LByteString
binaryEncode = encode

binaryDecode :: LByteString -> Either String (Vector Score)
binaryDecode lbs =
  case decodeOrFail lbs of
    Left (_, _, msg) -> Left msg
    Right (remainder, _, result)
      | BL.null remainder -> Right result
      | otherwise -> Left "unconsumed input"

instance Binary Score where
  put Score {..} = putText firstName *> putText lastName *> putMaybeWord8 value
  get = Score <$> getText <*> getText <*> getMaybeWord8

putText :: Text -> Put
putText t = do
  let bs = encodeUtf8 t
  putWord64be $ fromIntegral $ B.length bs
  putByteString bs

putMaybeWord8 :: Maybe Word8 -> Put
putMaybeWord8 Nothing  = putWord8 0
putMaybeWord8 (Just x) = putWord8 1 *> putWord8 x

getText :: Get Text
getText = do
  len <- getWord64be
  bs <- getByteString $ fromIntegral len
  case decodeUtf8' bs of
    Left e  -> fail $ "Invalid Utf8: " ++ show e
    Right t -> pure t

getMaybeWord8 :: Get (Maybe Word8)
getMaybeWord8 = do
  marker <- getWord8
  case marker of
    0 -> pure Nothing
    1 -> Just <$> getWord8
    _ -> fail $ "Invalid marker: " ++ show marker
