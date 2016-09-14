module Internal.BitOps
(
  fixedXOR
, repeatingKeyXOR
, hamming
) where

import qualified Data.ByteString as BS
import Data.Bits
import Internal.Uncons
import Data.Monoid
import Data.Word
import Debug.Trace

fixedXOR :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
fixedXOR bs1 bs2
  | BS.length bs1 /= BS.length bs2 = Nothing
  | otherwise                      = Just . BS.pack $ BS.zipWith xor bs1 bs2

repeatingKeyXOR :: BS.ByteString -> BS.ByteString -> BS.ByteString
repeatingKeyXOR k bs =
  let repeatingKey = cycle $ BS.unpack k
      unpackedBS   = BS.unpack bs
    in
      BS.pack $ zipWith xor repeatingKey unpackedBS

hamming :: BS.ByteString -> BS.ByteString -> Int
hamming bs1 bs2 =
  sum $ zipWith ((.) popCount . xor) (BS.unpack bs1) (BS.unpack bs2)
