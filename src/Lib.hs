{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( toBase64
    , fromBase64
    , sextets4
    , sextets3
    , sextets2
    ) where

import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Bits
import Data.Monoid
import Internal.TupleUtils
import qualified Data.Vector as V

newtype Base64Encoded =
  Base64Encoded {
    encoded :: BS.ByteString
  } deriving (Monoid, Show)

base64 :: V.Vector Char
base64 =
  V.fromList "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

uncons4 :: BS.ByteString -> Maybe (Char, Char, Char, Char, BS.ByteString)
uncons4 bs = uncons3 bs >>= (\(h, hh, hhh, t) ->
                              tupleConcat3 (h, hh, hhh) <$> BS.uncons t)

uncons3 :: BS.ByteString -> Maybe (Char, Char, Char, BS.ByteString)
uncons3 bs = uncons2 bs >>= (\(h, hh, t) -> tupleConcat (h, hh) <$> BS.uncons t)

uncons2 :: BS.ByteString -> Maybe (Char, Char, BS.ByteString)
uncons2 bs = BS.uncons bs >>= (\(h, t) -> tupleCons h <$> BS.uncons t)

mask3 :: Word8
mask3 = 3

mask15 :: Word8
mask15 = 15 -- 00001111

mask15Shifted :: Word8
mask15Shifted = 15 `shift` 2

mask63 :: Word8
mask63 = 63 -- 00111111

pad :: Word8
pad = 61    -- '='

decode4 :: Word8 -> Word8 -> Word8 -> Word8 -> BS.ByteString
decode4 a b c d =
  let f1 = a `shift` 2
      f2 = b `shift` (-4)
      f  = f1 .|. f2
      s1 = b .&. mask15
      s2 = s1 `shift` 4
      s3 = (c .&. mask15Shifted) `shift` (-2)
      s  = s2 .|. s3
      t  = ((c .&. mask3) `shift` 6) .|. (d .&. mask63)
    in BS.pack [f, s, t]

decode3 :: Word8 -> Word8 -> Word8 -> BS.ByteString
decode3 a b c = BS.take 2 $ decode4 a b c 0

decode2 :: Word8 -> Word8 -> BS.ByteString
decode2 a b = BS.take 1 $ decode4 a b 0 0

sextets4 :: Word8 -> Word8 -> Word8 -> Base64Encoded
sextets4 a b c =
  let f1 = a .&. mask63 `shift` 2
      fi = f1 `shift` (-2)
      s1 = a .&. 3
      s2 = b .&. 15  `shift` 4
      s3 = s2 `shift` (-4)
      se = s3 .|. s1 `shift` 4
      t1 = b .&. 15
      t2 = c .&. 3 `shift` 6
      t3 = t2 `shift` (-6)
      th = t3 .|. t1 `shift` 2
      fo = c .&. mask63
    in Base64Encoded $ BS.pack [fi, se, th, fo]

sextets3 :: Word8 -> Word8 -> Base64Encoded
sextets3 a b = Base64Encoded (BS.take 3 bytes <> BS.singleton pad)
  where (Base64Encoded bytes) = sextets4 a b 0

sextets2 :: Word8 -> Base64Encoded
sextets2 a = Base64Encoded (BS.take 2 bytes <> BS.replicate 2 pad)
  where (Base64Encoded bytes) = sextets4 a 0 0

toBase64 :: BS.ByteString -> Base64Encoded
toBase64 (uncons3   -> Just (h, hh, hhh, t)) = sextets4 h hh hhh <> toBase64 t
toBase64 (uncons2   -> Just (h, hh, t))      = sextets3 h hh     <> toBase64 t
toBase64 (BS.uncons -> Just (h, t))          = sextets2 h        <> toBase64 t
toBase64 _                                   = Base64Encoded BS.empty

fromBase64 :: Base64Encoded -> BS.ByteString
fromBase64 (Base64Encoded bytes) = decode bytes
  where
    decode (uncons4 -> Just (h, hh, hhh, hhhh, t))
      | hhhh == pad && hhh == pad  = decode2 h hh          <> decode t
      | hhhh == pad                = decode3 h hh hhh      <> decode t
      | otherwise                  = decode4 h hh hhh hhhh <> decode t
    decode _ = BS.empty
