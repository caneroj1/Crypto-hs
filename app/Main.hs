{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.ByteString as BS
import Data.Monoid

-- from the wikipedia page on Base64
text :: BS.ByteString
text = "Man is distinguished, not only by his reason, but by this singular passion from" <>
       " other animals, which is a lust of the mind, that by a perseverance of delight"  <>
       " in the continued and indefatigable generation of knowledge, exceeds the short"  <>
       " vehemence of any carnal pleasure."

encoded :: BS.ByteString
encoded = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz" <>
          "IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg" <>
          "dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu" <>
          "dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo" <>
          "ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="

check :: BS.ByteString -> BS.ByteString -> IO ()
check text encoding = do
  let myEncoding = toBase64 text
  print myEncoding
  print (show myEncoding == show encoded)
  let decoded = fromBase64 myEncoding
  print decoded
  print $ decoded == text

main :: IO ()
main = check text encoded
