module Internal.BitOps
(
  fixedXOR
) where

import qualified Data.ByteString as BS
import Data.Bits
import Internal.Uncons
import Data.Monoid
import Control.Applicative

fixedXOR :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
fixedXOR bs1 bs2
  | BS.length bs1 /= BS.length bs2 = Nothing
  | otherwise                      = Just . BS.pack $ BS.zipWith xor bs1 bs2
