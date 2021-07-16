{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Attoparsec (attoparsec) where

import           Conduit
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString as A
import           Data.Conduit.Attoparsec    as A
import           RIO
import qualified RIO.Vector                 as V
import           Types

attoparsec :: LByteString -> Either ParseError (Vector Score)
attoparsec = parseLazy (parser <* endOfInput)

parser :: Parser (Vector Score)
parser = do
  len <- anyWord64be
  V.replicateM (fromIntegral len) single

single :: Parser Score
single = Score <$> text <*> text <*> maybeWord8

text :: Parser Text
text = do
  len <- anyWord64be
  bs <- A.take (fromIntegral len)
  either (fail . show) pure $ decodeUtf8' bs

maybeWord8 :: Parser (Maybe Word8)
maybeWord8 = do
  marker <- anyWord8
  case marker of
    0 -> pure Nothing
    1 -> Just <$> anyWord8
    _ -> fail $ "Invalid Maybe marker: " ++ show marker

-- | Should ideally be provided by attoparsec itself
parseLazy :: Parser a -> LByteString -> Either ParseError a
parseLazy parser' lbs = runConduitPure $ sourceLazy lbs .| sinkParserEither parser'
