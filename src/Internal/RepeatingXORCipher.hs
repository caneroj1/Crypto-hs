module Internal.RepeatingXORCipher
(
  -- breakCipherRepeatingXOR
) where

import qualified Data.ByteString as BS
import Internal.BitOps
import Internal.TupleUtils
import Data.List
import Data.List.Split
import Data.Ord
import qualified Internal.XORCipher as XOR
import Debug.Trace

type KeyLen           = Int
type NormalizedWeight = Double

findNBestKeyLengths :: Int -> BS.ByteString -> [(NormalizedWeight, KeyLen)]
findNBestKeyLengths n bs =
  take n $ sortBy (comparing fst)
  [normalizeEditDistance bs keyLen | keyLen <- [2..40]]
  where
    normalizeEditDistance bs k =
      let firstN = BS.take k bs
          nextN  = BS.take k $ BS.drop k bs
        in (fromIntegral (hamming firstN nextN) / fromIntegral k, k)
