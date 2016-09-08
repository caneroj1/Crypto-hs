{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( toBase64
    , fromBase64
    , encodeW8
    , decodeW8
    , decode4
    ) where

import qualified Data.ByteString as BS
import Data.Word
import Data.Char
import Data.Bits
import Data.Monoid
import Internal.Uncons
import Data.Maybe
import qualified Data.Vector as V

newtype Base64Encoded =
  Base64Encoded {
    encoded :: BS.ByteString
  } deriving (Monoid)

instance Show Base64Encoded where
  show (Base64Encoded bytes) = show bytes

base64 :: V.Vector Char
base64 =
  V.fromList "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

encodeW8 :: Word8 -> Word8
encodeW8 w = fromIntegral . ord $ base64 V.! fromIntegral w

decodeW8 :: Word8 -> Word8
decodeW8 w = fromIntegral . fromJust $ V.findIndex (wChar ==) base64
  where
    wChar = chr $ fromIntegral w

pad :: Word8
pad = 61    -- '='

ePad :: Word8
ePad = 64

decode4 :: Word8 -> Word8 -> Word8 -> Word8 -> BS.ByteString
decode4 a b c d =
  let f  = (a `shift` 2) .|. (b `shift` (-4))
      s  = ((b .&. 15) `shift` 4) .|. ((c .&. 60) `shift` (-2))
      t  = ((c .&. 3) `shift` 6) .|. (d .&. 63)
    in BS.pack [f, s, t]

decode3 :: Word8 -> Word8 -> Word8 -> BS.ByteString
decode3 a b c = BS.take 2 $ decode4 a b c 0

decode2 :: Word8 -> Word8 -> BS.ByteString
decode2 a b = BS.take 1 $ decode4 a b 0 0

sextets4 :: Word8 -> Word8 -> Word8 -> BS.ByteString
sextets4 a b c =
  let f1 = a .&. 63 `shift` 2
      fi = f1 `shift` (-2)
      s1 = (b .&. 15  `shift` 4) `shift` (-4)
      se = s1 .|. (a .&. 3) `shift` 4
      t1 = (c .&. 3 `shift` 6) `shift` (-6)
      th = t1 .|. ( b .&. 15) `shift` 2
      fo = c .&. 63
    in BS.map encodeW8 $ BS.pack [fi, se, th, fo]

sextets3 :: Word8 -> Word8 -> BS.ByteString
sextets3 a b = BS.take 3 bytes <> BS.singleton pad
  where bytes = sextets4 a b 0

sextets2 :: Word8 -> BS.ByteString
sextets2 a = BS.take 2 bytes <> BS.replicate 2 pad
  where bytes = sextets4 a 0 0

toBase64 :: BS.ByteString -> Base64Encoded
toBase64 = Base64Encoded . encode
  where
    encode (uncons3   -> Just (h, hh, hhh, t)) = sextets4 h hh hhh <> encode t
    encode (uncons2   -> Just (h, hh, t))      = sextets3 h hh     <> encode t
    encode (BS.uncons -> Just (h, t))          = sextets2 h        <> encode t
    encode _                                   = BS.empty

fromBase64 :: Base64Encoded -> BS.ByteString
fromBase64 (Base64Encoded bytes) = decode $ BS.map decodeW8 bytes
  where
    decode (uncons4 -> Just (h, hh, hhh, hhhh, t))
      | hhhh == ePad && hhh == ePad = decode2 h hh          <> decode t
      | hhhh == ePad                = decode3 h hh hhh      <> decode t
      | otherwise                   = decode4 h hh hhh hhhh <> decode t
    decode _ = BS.empty
