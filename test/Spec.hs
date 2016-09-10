import Lib
import qualified Data.ByteString as BS
import Data.Word
import Data.Char
import Test.QuickCheck
import Data.Maybe

main :: IO ()
main = quickCheck prop_getSameStringBack

prop_getSameStringBack :: String -> Bool
prop_getSameStringBack string =
  let bytes   = map (fromIntegral . ord) string
      packed  = BS.pack bytes
      encoded = toBase64 packed
      decoded = fromBase64 encoded
    in BS.unpack decoded == bytes
    
