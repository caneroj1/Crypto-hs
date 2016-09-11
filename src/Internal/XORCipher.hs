{-# LANGUAGE OverloadedStrings #-}

module Internal.XORCipher
(
  breakCipher
) where

import Internal.BitOps
import Internal.Conversions
import Data.HexString (toText, HexString, hexString, toBytes)
import Data.Text (Text)
import qualified Data.Text as Text (chunksOf, unpack, length)
import qualified Data.ByteString as BS
import Data.List
import Control.Arrow ((&&&))
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Ord

trackBytes :: HexString -> [(Text, Down Int)]
trackBytes = sortBy (comparing snd)         .
             map (head &&& (Down . length)) .
             group                          .
             sort                           .
             Text.chunksOf 2                .
             toText

mostFrequentLetters :: [Word8]
mostFrequentLetters = BS.unpack "etaoi"

getCipher :: Text -> Word8 -> Word8
getCipher byteInHex keyByte
  | Text.length byteInHex /= 2 = undefined
  | otherwise                  =
    xor keyByte . head . BS.unpack $ hexTextToByteString byteInHex

-- try a few different things here I guess.
-- we have a list of a few of the most frequent characters in the english
-- alphabet in mostFrequentLetters.
-- using trackBytes, we can get an ordered list of the most frequently
-- occuring bytes (in hex) from the hexString.
-- for now:
-- using the most frequently occuring byte in the hex string, try assuming
-- that that byte was one of the most frequent letters.
-- for each of those letters, calculate what the cipher character must have been
-- to make that character become the byte we're looking at (just a XOR op).
breakCipher :: HexString -> [BS.ByteString]
breakCipher hex = catMaybes $ do
  (byte, _) <- trackBytes hex
  c         <- mostFrequentLetters
  [bytestring `fixedXOR` asBuffer (BS.length bytestring) (getCipher byte c)]
  where bytestring = toBytes hex
        asBuffer l =  BS.pack . replicate l
