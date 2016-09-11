module Internal.Conversions
(
  textToByteString
, hexTextToByteString
, hexToBytes
) where

import Data.HexString (toText, HexString, hexString, toBytes)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack)
import Data.Char (ord)

textToByteString :: Text -> ByteString
textToByteString = BS.pack . map (fromIntegral . ord) . Text.unpack

hexTextToByteString :: Text -> ByteString
hexTextToByteString = toBytes . hexString . textToByteString

hexToBytes :: ByteString -> ByteString
hexToBytes = toBytes . hexString
