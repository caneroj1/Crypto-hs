{-# LANGUAGE OverloadedStrings #-}

module Internal.XORCipher
(
  breakCipher
) where

import Internal.BitOps
import Data.HexString (toText, HexString, hexString, toBytes)
import qualified Data.ByteString as BS
import Data.List
import Control.Arrow ((&&&))
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Ord

class Bytes a where
  getBytes :: a -> [Word8]

instance Bytes HexString where
  getBytes = BS.unpack . toBytes

instance Bytes BS.ByteString where
  getBytes = BS.unpack

trackBytes :: (Bytes a) => a -> [(Word8, Down Int)]
trackBytes = sortBy (comparing snd)         .
             map (head &&& (Down . length)) .
             group                          .
             sort                           .
             getBytes

mostFrequentLetters :: [Word8]
mostFrequentLetters = BS.unpack "etaoinshrdlu"

numMostFrequentLetters = length mostFrequentLetters

maxToLookAt = 5

-- hmm. how to rank a bytestring?
-- i guess we want to look at the most frequent bytes and compare them to
-- mostFrequentLetters. we'll look at the 5 most frequent bytes in a bytestring
-- and do the following:
-- 1. see if that byte occurs in mostFrequentLetters.
--    1a. if it does, we want the points we get to be weighted by the index
--        in mostFrequentLetters as well as by the index this byte is in
--        our bytestring. so: give more points to frequently occuring bytes in
--        our bytestring that are closer to the front of the mostFrequentLetters
--        list.
--    1b. if it doesn't occur in the list, give it 0 points.
rank :: BS.ByteString -> Double
rank str = sum $ do
  (idx, (w8, Down freq)) <- take maxToLookAt . zip [0..] $ trackBytes str
  [maybe deduct (rankingFunction idx freq) (elemIndex w8 mostFrequentLetters)]
  where
    rankingFunction idxInString frequency idxInFreqList =
      fromIntegral frequency *
      (fromIntegral (maxToLookAt - idxInString)              * 0.4 +
       fromIntegral (numMostFrequentLetters - idxInFreqList) * 0.6)
    deduct = -1.0

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
breakCipher :: HexString -> [(BS.ByteString, Down Double)]
breakCipher hex = take maxToLookAt           .
                  uniqueSorted               .
                  sortBy (comparing snd)     .
                  map (id &&& (Down . rank)) .
                  catMaybes $ do
                    (byte, _) <- trackBytes hex
                    c         <- mostFrequentLetters
                    let buffer = asBuffer (BS.length bytestring) (byte `xor` c)
                    [bytestring `fixedXOR` buffer]
  where bytestring = toBytes hex
        asBuffer l = BS.pack . replicate l

uniqueSorted :: (Eq a) => [a] -> [a]
uniqueSorted = map head . group
