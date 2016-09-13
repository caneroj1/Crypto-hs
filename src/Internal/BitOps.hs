module Internal.BitOps
(
  fixedXOR
, repeatingKeyXOR
) where

import qualified Data.ByteString as BS
import Data.Bits
import Internal.Uncons
import Data.Monoid

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
