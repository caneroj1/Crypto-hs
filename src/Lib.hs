{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( toBase64
    , sextets4
    , sextets3
    , sextets2
    ) where

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.Monoid

newtype Base64Encoded =
  Base64Encoded {
    encoded :: BS.ByteString
  } deriving (Monoid, Show)

tupleCons :: c -> (a, b) -> (c, a, b)
tupleCons c (a, b) = (c, a, b)

tupleConcat :: (a, b) -> (c, d) -> (a, b, c, d)
tupleConcat (a, b) (c, d) = (a, b, c, d)

uncons3 :: BS.ByteString -> Maybe (Word8, Word8, Word8, BS.ByteString)
uncons3 bs = uncons2 bs >>= (\(h, hh, t) -> tupleConcat (h, hh) <$> BS.uncons t)

uncons2 :: BS.ByteString -> Maybe (Word8, Word8, BS.ByteString)
uncons2 bs = BS.uncons bs >>= (\(h, t) -> tupleCons h <$> BS.uncons t)

mask63 :: Word8
mask63 = 63 -- 00111111

mask15 :: Word8
mask15 = 15 -- 00001111

pad :: Word8
pad = 61    -- '='

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
